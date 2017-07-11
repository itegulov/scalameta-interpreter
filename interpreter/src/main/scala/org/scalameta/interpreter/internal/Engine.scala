package org.scalameta.interpreter.internal

import org.scalameta.interpreter.internal.environment._
import com.typesafe.scalalogging.Logger
import org.scalameta.interpreter.ScalametaMirror
import org.scalameta.interpreter.ScalametaMirror._

import scala.collection.immutable
import scala.collection.immutable.ListMap
import scala.meta.Term.Block
import scala.meta._
import scala.runtime.BoxesRunTime
import scala.util.{Failure, Success, Try}

object Engine {
  private val logger = Logger[Engine.type]

  def eval(tree: Tree)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) =
    eval(tree, Env(List(Map.empty), Map.empty, ClassTable(Map.empty), ListMap.empty))

  def eval(tree: Tree, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = tree match {
    case literal: Lit                    => evalLiteral(literal, env)
    case definition: Defn                => evalLocalDef(definition, env)
    case declaration: Decl               => InterpreterRef.wrap((), env, t"Unit") // FIXME: should probably do something with them
    case block: Block                    => evalBlock(block, env)
    case name: Term.Name                 => evalName(name, env)
    case ifTerm: Term.If                 => evalIf(ifTerm, env)
    case apply: Term.Apply               => evalApply(apply, env)
    case Term.ApplyInfix(lhs, op, _, xs) => evalApply(Term.Apply(Term.Select(lhs, op), xs), env)
    case assignment: Term.Assign         => evalAssignment(assignment, env)
    case newTerm: Term.New               => evalNew(newTerm, env)
    case select: Term.Select             => evalSelect(select, env)
    case template: Template              => evalTemplate(template, env)
  }

  def evalTemplate(template: Template, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    var resEnv = env
    for (arg <- template.stats.getOrElse(Seq.empty)) {
      val (argRef, newEnv) = eval(arg, resEnv)
      resEnv = newEnv
      argRef
    }
    InterpreterRef.wrap((), resEnv, t"Unit")
  }

  def evalSelect(select: Term.Select, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (qualRef, env1) = eval(select.qual, env)
    env1.heap.get(qualRef) match {
      case Some(InterpreterPrimitive(value)) =>
        import scala.reflect.runtime.universe._
        val m      = runtimeMirror(value.getClass.getClassLoader).reflect(value)
        val symbol = m.symbol.typeSignature.member(TermName(select.name.value))
        val result = m.reflectMethod(symbol.asMethod)()
        val ref    = InterpreterJvmRef(null)
        (ref, env.extend(ref, InterpreterPrimitive(result)))
      case Some(InterpreterObject(_, fields)) =>
        fields.get(select.name.symbol) match {
          case Some(value) => (value, env1)
          case None        => sys.error(s"Unknown field ${select.name} for object $qualRef")
        }
    }
  }

  def evalNew(newTerm: Term.New, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val ctorCall = newTerm.templ.parents.head // FIXME: Is it possible to have more than one ctor call?
    ctorCall match {
      case Term.Apply(className, argTerms) =>
        var resEnv = env
        val argRefs = for (arg <- argTerms) yield {
          val (argRef, newEnv) = eval(arg, resEnv)
          resEnv = newEnv
          argRef
        }
        resEnv.classTable.table.get(className.symbol) match {
          case Some(classInfo) => classInfo.constructor(argRefs, resEnv)
          case None            => sys.error(s"Unknown class $className")
        }
    }
  }

  def evalAssignment(assignment: Term.Assign, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (assignmentRef, env1) = eval(assignment.rhs, env)
    assignment.lhs match {
      case name: Term.Name => InterpreterRef.wrap((), env1.extend(name.symbol, assignmentRef), t"Unit")
      case Term.Select(qual, name) =>
        val (ref, env2) = eval(qual, env1)
        env2.heap.get(ref) match {
          case Some(InterpreterPrimitive(value)) => sys.error("Can not mutate primitives")
          case Some(obj @ InterpreterObject(_, fields)) if fields.contains(name.symbol) =>
            val newObj = obj.extend(name.symbol, assignmentRef)
            InterpreterRef.wrap((), env2.extend(ref, newObj), t"Unit")
          case Some(InterpreterObject(_, _)) => sys.error(s"Unknown field ${name.value} for $ref")
          case None                       => sys.error("Illegal state")
        }
      case _ => sys.error(s"Can not interpret unrecognized tree ${assignment.lhs}")
    }
  }

  def evalApply(apply: Term.Apply, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    var resEnv = env
    val argRefs = for (arg <- apply.args) yield {
      val (argRef, newEnv) = eval(arg, resEnv)
      resEnv = newEnv
      argRef
    }
    apply.fun match {
      case Term.Select(qual, name) =>
        val (qualRef, env1) = eval(qual, resEnv)
        env1.heap.get(qualRef) match {
          case Some(InterpreterPrimitive(value)) =>
            val argValues   = argRefs.map(env1.heap.apply).map(_.asInstanceOf[InterpreterPrimitive]).map(_.value)
            name.symbol match {
              case ScalametaMirror.AnyEquals => argValues match {
                case Seq(arg) => InterpreterRef.wrap(value == arg, env1, t"Boolean")
                case _ => sys.error(s"Expected one argument for equals(), but got $argValues")
              }
              case ScalametaMirror.AnyHashcode =>
                if (argValues.nonEmpty) {
                  sys.error(s"Expected no arguments for hashCode, but got $argValues")
                }
                InterpreterRef.wrap(value.hashCode(), env1, t"Int")
              case ScalametaMirror.`Any==` => argValues match {
                case Seq(arg) => InterpreterRef.wrap(value == arg, env1, t"Boolean")
                case _ => sys.error(s"Expected one argument for ==, but got $argValues")
              }
              case ScalametaMirror.`Any!=` => argValues match {
                case Seq(arg) => InterpreterRef.wrap(value != arg, env1, t"Boolean")
                case _ => sys.error(s"Expected one argument for !=, but got $argValues")
              }
              case _ =>
                val runtimeName = toRuntimeName(name.value)
                val allFuns     = classOf[BoxesRunTime].getDeclaredMethods.filter(_.getName == runtimeName)
                val fun         = allFuns.head
                val result      = fun.invoke(null, (value +: argValues).asInstanceOf[Seq[AnyRef]]: _*)
                val ref         = InterpreterJvmRef(null)
                (ref, env.extend(ref, InterpreterPrimitive(result)))
            }
          case Some(InterpreterObject(className, fields)) =>
            fields.get(name.symbol) match {
              case Some(ref) =>
                Try(ref.asInstanceOf[InterpreterFunctionRef]) match {
                  case Success(fun) => fun(argRefs, env1.addThis(className, qualRef))
                  case Failure(_) =>
                    sys.error(s"Tried to call ${name.value}, but it is not a function")
                }
              case None => sys.error(s"Unknown field $name for object $qualRef")
            }
          case _ => sys.error("Illegal state")
        }
      case name: Term.Name =>
        resEnv.stack.head.get(name.symbol) match {
          case Some(funRef: InterpreterFunctionRef) => funRef(argRefs, resEnv)
          case Some(x)                              => sys.error(s"Expected function, but got $x")
          case None                                 =>
            import scala.reflect.runtime.{universe => ru}
            val m = ru.runtimeMirror(Predef.getClass.getClassLoader)
            val module = m.staticModule("scala.Predef")
            val im = m.reflectModule(module)
            val alternatives = im.symbol.info.decl(ru.TermName(name.value)).asTerm.alternatives.map(_.asMethod)
            val method = alternatives.filter(_.paramLists.head.size == argRefs.size).head
            val objScalametaMirror = m.reflect(im.instance)
            val ref = InterpreterJvmRef(null)

            val argsValues = argRefs.map(resEnv.heap.apply).map {
              case InterpreterPrimitive(value) => value
              case InterpreterObject(_, fields) => InterpreterDynamic(fields)
              case InterpreterWrappedJvm(value) => value
            }

            val res = InterpreterWrappedJvm(objScalametaMirror.reflectMethod(method)(argsValues: _*))
            (ref, resEnv.extend(ref, res))
        }
    }
  }

  def evalIf(ifTerm: Term.If, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
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

  def evalName(name: Term.Name, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) =
    env.stack.head.get(name.symbol) match {
      case Some(ref) =>
        (ref, env)
      case None      =>
        for ((_, classRef) <- env.thisContext) {
          val obj = env.heap(classRef).asInstanceOf[InterpreterObject]
          obj.fields.get(name.symbol) match {
            case Some(ref) => return (ref, env)
            case _ =>
          }
        }
        sys.error(s"Unknown reference $name")
    }

  def evalBlock(block: Block, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
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

  def evalLocalDef(definition: Defn, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) =
    definition match {
      case Defn.Val(mods, pats, _, rhs) =>
        val (res, env1) = eval(rhs, env)
        definition.parent match {
          case Some(Template(_, _, _, _)) =>
            val (_, ref) = env.thisContext.head
            var resObj = env1.heap(ref).asInstanceOf[InterpreterObject]
            for (pat <- pats) {
              pat match {
                case Pat.Var.Term(termName) =>
                  resObj = resObj.extend(termName.symbol, res)
              }
            }
            InterpreterRef.wrap((), env1.extend(ref, resObj), t"Unit")
          case _ =>
            var resEnv      = env1
            for (pat <- pats) {
              pat match {
                case Pat.Var.Term(name) =>
                  resEnv = resEnv.extend(name.symbol, res)
              }
            }
            InterpreterRef.wrap((), resEnv, t"Unit")
        }
      case Defn.Var(mods, pats, optTpe, optRhs) =>
        val (res, env1) = (optTpe, optRhs) match {
          case (_, Some(rhs))    => eval(rhs, env)
          case (Some(tpe), None) => defaultValue(tpe, env)
          case (None, None)      => sys.error("Unreachable")
        }
        definition.parent match {
          case Some(Template(_, _, _, _)) =>
            val (_, ref) = env.thisContext.head
            var resObj = env1.heap(ref).asInstanceOf[InterpreterObject]
            for (pat <- pats) {
              pat match {
                case Pat.Var.Term(termName) =>
                  resObj = resObj.extend(termName.symbol, res)
              }
            }
            InterpreterRef.wrap((), env1.extend(ref, resObj), t"Unit")
          case _ =>
            var resEnv = env1
            for (pat <- pats) {
              pat match {
                case Pat.Var.Term(name) =>
                  resEnv = resEnv.extend(name.symbol, res)
              }
            }
            InterpreterRef.wrap((), resEnv, t"Unit")
        }
      case Defn.Def(mods, name, tparams, paramss, tpe, body) =>
        definition.parent match {
          case Some(Template(_, _, _, _)) =>
            val (_, ref) = env.thisContext.head
            val obj = env.heap(ref).asInstanceOf[InterpreterObject]
            val funRef = InterpreterFunctionRef(paramss, tparams, body, env)
            val newObj = obj.extend(name.symbol, funRef)
            InterpreterRef.wrap((), env.extend(name.symbol, funRef).extend(ref, newObj), t"Unit")
          case _ =>
            val funRef = InterpreterFunctionRef(paramss, tparams, body, env)
            InterpreterRef.wrap((), env.extend(name.symbol, funRef), t"Unit")
        }
      case Defn.Trait(mods, name, tparams, ctor, templ) =>
        require(ctor.paramss.isEmpty, "Trait constructor should not have any parameters")
        ???
      case Defn.Class(mods, name, tparams, ctor, templ) =>
        val constructors = for (parent <- templ.parents) yield {
          parent match {
            case Term.Apply(className, args) =>
              env.classTable.table.get(className.symbol) match {
                case Some(classInfo) => (classInfo.constructor, args)
                case None            => sys.error(s"Unknown parent class: $className")
              }
            case className: Ctor.Ref.Name =>
              env.classTable.table.get(className.symbol) match {
                case Some(classInfo) => (classInfo.constructor, Seq.empty)
                case None            => sys.error(s"Unknown parent class: $className")
              }
          }
        }
        val ctorRef = InterpreterCtorRef(
          name.symbol,
          ctor.paramss,
          null,
          templ,
          env,
          constructors
        )
        val resEnv = env.addClass(name.symbol, ClassInfo(ctorRef))
        InterpreterRef.wrap((), resEnv, t"Unit")
      case Defn.Macro(mods, name, tparams, paramss, decltpe, body) =>
        sys.error("Macroses are not supported")
      case Defn.Object(mods, name, templ) =>
        val (_, env1) =
          eval(Block(templ.stats.getOrElse(immutable.Seq.empty)), env.pushFrame(Map.empty))
        val obj    = InterpreterObject(name.symbol, env1.stack.head)
        val ref    = InterpreterJvmRef(Type.Name(name.value))
        val resEnv = Env(env1.stack.tail, env1.heap + (ref -> obj), env1.classTable, env1.thisContext)
        InterpreterRef.wrap((), resEnv.extend(name.symbol, ref), t"Unit")
      case Defn.Type(mods, name, tparams, body) =>
        logger.info("Ignoring type alias definition")
        InterpreterRef.wrap((), env, t"Unit")
    }
}
