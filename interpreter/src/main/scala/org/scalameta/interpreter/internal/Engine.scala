package org.scalameta.interpreter.internal

import org.scalameta.interpreter.internal.environment._
import com.typesafe.scalalogging.Logger

import scala.collection.immutable
import scala.meta.Term.Block
import scala.meta._
import scala.runtime.BoxesRunTime
import scala.util.{Failure, Success, Try}

object Engine {
  private val logger = Logger[Engine.type]

  def eval(tree: Tree): (InterpreterRef, Env) =
    eval(tree, Env(List(Map.empty), Map.empty, ClassTable(Map.empty)))

  def eval(tree: Tree, env: Env): (InterpreterRef, Env) = tree match {
    case literal: Lit                    => evalLiteral(literal, env)
    case definition: Defn                => evalLocalDef(definition, env)
    case block: Block                    => evalBlock(block, env)
    case name: Term.Name                 => evalName(name, env)
    case ifTerm: Term.If                 => evalIf(ifTerm, env)
    case apply: Term.Apply               => evalApply(apply, env)
    case Term.ApplyInfix(lhs, op, _, xs) => evalApply(Term.Apply(Term.Select(lhs, op), xs), env)
    case assignment: Term.Assign         => evalAssignment(assignment, env)
    case newTerm: Term.New               => evalNew(newTerm, env)
    case select: Term.Select             => evalSelect(select, env)
  }

  def evalSelect(select: Term.Select, env: Env): (InterpreterRef, Env) = {
    val (qualRef, env1) = eval(select.qual, env)
    env1.heap.get(qualRef) match {
      case Some(InterpreterPrimitive(value)) =>
        import scala.reflect.runtime.universe._
        val m      = runtimeMirror(value.getClass.getClassLoader).reflect(value)
        val symbol = m.symbol.typeSignature.member(TermName(select.name.value))
        val result = m.reflectMethod(symbol.asMethod)()
        val ref    = InterpreterJvmRef(null)
        (ref, env.extend(ref, InterpreterPrimitive(result)))
      case Some(InterpreterObject(fields)) =>
        fields.get(select.name.value) match {
          case Some(value) => (value, env1)
          case None        => sys.error(s"Unknown field ${select.name} for object $qualRef")
        }
    }
  }

  def evalNew(newTerm: Term.New, env: Env): (InterpreterRef, Env) = {
    val ctorCall = newTerm.templ.parents.head // FIXME: Is it possible to have more than one ctor call?
    ctorCall match {
      case Term.Apply(Ctor.Ref.Name(className), argTerms) =>
        var resEnv = env
        val argRefs = for (arg <- argTerms) yield {
          val (argRef, newEnv) = eval(arg, resEnv)
          resEnv = newEnv
          argRef
        }
        resEnv.classTable.table.get(ClassName(className)) match {
          case Some(classInfo) => classInfo.constructor(argRefs, resEnv)
          case None            => sys.error(s"Unknown class $className")
        }
    }
  }

  def evalAssignment(assignment: Term.Assign, env: Env): (InterpreterRef, Env) = {
    val (assignmentRef, env1) = eval(assignment.rhs, env)
    assignment.lhs match {
      case Term.Name(name) => InterpreterRef.wrap((), env1.extend(name, assignmentRef), t"Unit")
      case _               => sys.error(s"Can not interpret unrecognized tree ${assignment.lhs}")
    }
  }

  def evalApply(apply: Term.Apply, env: Env): (InterpreterRef, Env) = {
    var resEnv = env
    val argRefs = for (arg <- apply.args) yield {
      val (argRef, newEnv) = eval(arg, resEnv)
      resEnv = newEnv
      argRef
    }
    apply.fun match {
      case Term.Select(qual, name) =>
        val (qualRef, env1) = eval(qual, resEnv)
        val argValues =
          argRefs.map(env1.heap.apply).map(_.asInstanceOf[InterpreterPrimitive]).map(_.value)
        val argClasses = argValues.map(_.getClass)
        env1.heap.get(qualRef) match {
          case Some(InterpreterPrimitive(value)) =>
            val runtimeName = toRuntimeName(name.value)
            val allFuns     = classOf[BoxesRunTime].getDeclaredMethods.filter(_.getName == runtimeName)
            val fun         = allFuns.head
            val result      = fun.invoke(null, (value +: argValues).asInstanceOf[Seq[AnyRef]]: _*)
            val ref         = InterpreterJvmRef(null)
            (ref, env.extend(ref, InterpreterPrimitive(result)))
          case Some(InterpreterObject(fields)) =>
            fields.get(name.value) match {
              case Some(ref) =>
                Try(ref.asInstanceOf[InterpreterFunctionRef]) match {
                  case Success(fun) => fun(argRefs, env1)
                  case Failure(_) =>
                    sys.error(s"Tried to call ${name.value}, but it is not a function")
                }
              case None => sys.error(s"Unknown field $name for object $qualRef")
            }
          case _ => sys.error("Illegal state")
        }
    }
  }

  def evalIf(ifTerm: Term.If, env: Env): (InterpreterRef, Env) = {
    val (condRef, env1) = eval(ifTerm.cond, env)
    val condEvaluation = env1.heap.get(condRef) match {
      case Some(result) =>
        Try(result.asInstanceOf[InterpreterPrimitive]) match {
          case Success(primitive) =>
            Try(primitive.value.asInstanceOf[Boolean]) match {
              case Success(value) => value
              case Failure(_)     => sys.error("`If`'s conditions should have type `Boolean`")
            }
          case Failure(_) => sys.error("`If`'s conditions should be primitive") // TODO: is it?
        }
      case None => sys.error("Illegal state")
    }
    if (condEvaluation) {
      eval(ifTerm.thenp, env1)
    } else {
      eval(ifTerm.elsep, env1)
    }
  }

  def evalName(name: Term.Name, env: Env): (InterpreterRef, Env) =
    env.stack.head.get(name.value) match {
      case Some(ref) => (ref, env)
      case None      => sys.error(s"Unknown reference $name")
    }

  def evalBlock(block: Block, env: Env): (InterpreterRef, Env) = {
    var result = InterpreterRef.wrap((), env, t"Unit")
    for (stat <- block.stats) {
      val (_, resEnv) = result
      result = eval(stat, resEnv)
    }
    result
  }

  def evalLiteral(literal: Lit, env: Env): (InterpreterRef, Env) = literal match {
    case Lit.Byte(value)    => InterpreterRef.wrap(value, env, t"Byte")
    case Lit.Short(value)   => InterpreterRef.wrap(value, env, t"Short")
    case Lit.Char(value)    => InterpreterRef.wrap(value, env, t"Char")
    case Lit.Int(value)     => InterpreterRef.wrap(value, env, t"Int")
    case Lit.Long(value)    => InterpreterRef.wrap(value, env, t"Long")
    case Lit.Float(value)   => InterpreterRef.wrap(value.toFloat, env, t"Float")
    case Lit.Double(value)  => InterpreterRef.wrap(value.toDouble, env, t"Double")
    case Lit.Boolean(value) => InterpreterRef.wrap(value, env, t"Boolean")
    case Lit.Unit(value)    => InterpreterRef.wrap(value, env, t"Unit")
    case Lit.String(value)  => InterpreterRef.wrap(value, env, t"String")
    case Lit.Null(value)    => InterpreterRef.wrap(value, env, t"Any")
    case Lit.Symbol(value)  => sys.error(s"Can not interpret symbol tree $literal")
    case _                  => sys.error(s"Can not interpret unrecognized tree $literal")
  }

  def defaultValue(tpe: Type, env: Env): (InterpreterRef, Env) = tpe match {
    case t"Byte"    => InterpreterRef.wrap(0.toByte, env, tpe)
    case t"Short"   => InterpreterRef.wrap(0.toShort, env, tpe)
    case t"Char"    => InterpreterRef.wrap(0.toChar, env, tpe)
    case t"Int"     => InterpreterRef.wrap(0, env, tpe)
    case t"Long"    => InterpreterRef.wrap(0l, env, tpe)
    case t"Float"   => InterpreterRef.wrap(0.0f, env, tpe)
    case t"Double"  => InterpreterRef.wrap(0.0d, env, tpe)
    case t"Boolean" => InterpreterRef.wrap(false, env, tpe)
    case t"Unit"    => InterpreterRef.wrap((), env, tpe)
    case _          => InterpreterRef.wrap(null, env, tpe)
  }

  def toRuntimeName(name: String): String = name match {
    case "+" => "add"
    case "-" => "subtract"
    case "*" => "multiply"
    case "/" => "divide"
    case x   => x
  }

  def evalLocalDef(definition: Defn, env: Env): (InterpreterRef, Env) =
    definition match {
      case Defn.Val(mods, pats, _, rhs) =>
        val (res, env1) = eval(rhs, env)
        var resEnv      = env1
        for (pat <- pats) {
          pat match {
            case Pat.Var.Term(name) =>
              resEnv = resEnv.extend(name.value, res)
          }
        }
        InterpreterRef.wrap((), resEnv, t"Unit")
      case Defn.Var(mods, pats, optTpe, optRhs) =>
        val (res, env1) = (optTpe, optRhs) match {
          case (_, Some(rhs))    => eval(rhs, env)
          case (Some(tpe), None) => defaultValue(tpe, env)
          case (None, None)      => sys.error("Unreachable")
        }
        var resEnv = env1
        for (pat <- pats) {
          pat match {
            case Pat.Var.Term(name) =>
              resEnv = resEnv.extend(name.value, res)
          }
        }
        InterpreterRef.wrap((), resEnv, t"Unit")
      case Defn.Def(mods, name, tparams, paramss, tpe, body) =>
        val funRef = InterpreterFunctionRef(paramss, tparams, body, env)
        (funRef, env.extend(name.value, funRef))
      case Defn.Trait(mods, name, tparams, ctor, templ) =>
        ???
      case Defn.Class(mods, name, tparams, ctor, templ) =>
        val ctorRef = InterpreterCtorRef(
          ctor.paramss,
          null,
          Block(templ.stats.getOrElse(immutable.Seq.empty[Stat])),
          env
        )
        val resEnv = env.addClass(ClassName(name.value), ClassInfo(ctorRef))
        InterpreterRef.wrap((), resEnv, t"Unit")
      case Defn.Macro(mods, name, tparams, paramss, decltpe, body) =>
        sys.error("Macroses are not supported")
      case Defn.Object(mods, name, templ) =>
        ???
      case Defn.Type(mods, name, tparams, body) =>
        logger.info("Ignoring type alias definition")
        InterpreterRef.wrap((), env, t"Unit")
    }
}
