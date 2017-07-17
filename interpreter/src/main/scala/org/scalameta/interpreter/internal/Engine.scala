package org.scalameta.interpreter.internal

import org.scalameta.interpreter.internal.environment._
import com.typesafe.scalalogging.Logger
import org.scalameta.interpreter.ScalametaMirror
import org.scalameta.interpreter.ScalametaMirror._
import org.scalameta.interpreter.internal.flow.exceptions.{InterpreterException, ReturnException}

import scala.collection.immutable
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer
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
    case template: Template              => evalTemplate(template, env)
    case name: Term.Name                 => evalName(name, env)
    case term: Term                      => evalTerm(term, env)
  }

  def evalTerm(term: Term, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = term match {
    case apply: Term.Apply               => evalApply(apply, env)
    case Term.ApplyInfix(lhs, op, _, xs) => evalApply(Term.Apply(Term.Select(lhs, op), xs), env)
    case applyType: Term.ApplyType       => ???
    case ascribe: Term.Ascribe           => evalAscribe(ascribe, env)
    case assignment: Term.Assign         => evalAssignment(assignment, env)
    case doTerm: Term.Do                 => evalDoWhile(doTerm, env)
    case eta: Term.Eta                   => ???
    case forTerm: Term.For               => evalFor(forTerm, env)
    case forTerm: Term.ForYield          => evalForYield(forTerm, env)
    case function: Term.Function         => ???
    case ifTerm: Term.If                 => evalIf(ifTerm, env)
    case interpolate: Term.Interpolate   => ???
    case termMatch: Term.Match           => evalMatch(termMatch, env)
    case newTerm: Term.New               => evalNew(newTerm, env)
    case function: Term.PartialFunction  => ???
    case placeholder: Term.Placeholder   => ???
    case returnTerm: Term.Return         => evalReturn(returnTerm, env)
    case throwTerm: Term.Throw           => evalThrow(throwTerm, env)
    case tryCatchTerm: Term.TryWithCases => evalTry(tryCatchTerm, env)
    case tryCatchTerm: Term.TryWithTerm  => ??? // TODO: find out what is try with term
    case tuple: Term.Tuple               => evalTuple(tuple, env)
    case update: Term.Update             => ???
    case whileTerm: Term.While           => evalWhile(whileTerm, env)
    case select: Term.Select             => evalSelect(select, env)
    case xml: Term.Xml                   => ???
  }

  def evalForYield(forTerm: Term.ForYield, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {

    case object NoValue

    def evalForYieldRec(enums: Seq[Enumerator], lEnv: Env): (InterpreterRef, Env) = {
      enums match {
        case Enumerator.Generator(pat, rhs) :: tail =>
          val (rhsRef, rhsEnv) = eval(rhs, lEnv)
          val iterableRhs = rhsRef.reify(rhsEnv).asInstanceOf[Iterable[_]]
          pat match {
            case Pat.Var.Term(name) =>
              var resEnv = rhsEnv
              val result = ArrayBuffer.empty[Any]
              for (oneRhs <- iterableRhs) {
                val oneRhsRef = InterpreterJvmRef(null)
                resEnv = resEnv.extend(oneRhsRef, InterpreterWrappedJvm(oneRhs))
                val (newRef, newEnv) = evalForYieldRec(tail, resEnv)
                resEnv = newEnv
                val newVal = newRef.reify(newEnv)
                if (newVal != NoValue) {
                  newVal match {
                    case iterable: Iterable[_] => result ++= iterable
                    case other                 => result += other
                  }
                }
              }
              InterpreterRef.wrapJvm(result, resEnv, t"Unit")
          }
        case Enumerator.Guard(cond) :: tail =>
          val (condRef, condEnv) = eval(cond, lEnv)
          if (condRef.reifyBoolean(condEnv)) {
            evalForYieldRec(tail, condEnv)
          } else {
            InterpreterRef.wrapJvm(NoValue, condEnv, null)
          }
        case Enumerator.Val(pat, rhs) :: tail =>
          val (rhsRef, rhsEnv) = eval(rhs, lEnv)
          pat match {
            case Pat.Var.Term(name) =>
              evalForYieldRec(tail, rhsEnv.extend(name.symbol, rhsRef))
          }
        case Nil =>
          val (_, newEnv) = eval(forTerm.body, lEnv)
          InterpreterRef.wrap((), newEnv, t"Unit")
      }
    }
    evalForYieldRec(forTerm.enums, env)
  }

  def evalFor(forTerm: Term.For, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    def evalForRec(enums: Seq[Enumerator], lEnv: Env): (InterpreterRef, Env) = {
      enums match {
        case Enumerator.Generator(pat, rhs) :: tail =>
          val (rhsRef, rhsEnv) = eval(rhs, lEnv)
          val iterableRhs = rhsRef.reify(rhsEnv).asInstanceOf[Iterable[_]]
          pat match {
            case Pat.Var.Term(name) =>
              var resEnv = rhsEnv
              for (oneRhs <- iterableRhs) {
                val oneRhsRef = InterpreterJvmRef(null)
                resEnv = resEnv.extend(oneRhsRef, InterpreterWrappedJvm(oneRhs))
                val (_, newEnv) = evalForRec(tail, resEnv)
                resEnv = newEnv
              }
              InterpreterRef.wrap((), resEnv, t"Unit")
          }
        case Enumerator.Guard(cond) :: tail =>
          val (condRef, condEnv) = eval(cond, lEnv)
          if (condRef.reifyBoolean(condEnv)) {
            evalForRec(tail, condEnv)
          } else {
            InterpreterRef.wrap((), condEnv, t"Unit")
          }
        case Enumerator.Val(pat, rhs) :: tail =>
          val (rhsRef, rhsEnv) = eval(rhs, lEnv)
          pat match {
            case Pat.Var.Term(name) =>
              evalForRec(tail, rhsEnv.extend(name.symbol, rhsRef))
          }
        case Nil =>
          val (_, newEnv) = eval(forTerm.body, lEnv)
          InterpreterRef.wrap((), newEnv, t"Unit")
      }
    }
    evalForRec(forTerm.enums, env)
  }

  def evalAscribe(ascribe: Term.Ascribe, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (ref, newEnv) = eval(ascribe.expr, env)
    val value = ref.reify(newEnv)
    val classLoader = getClass.getClassLoader
    val clazz = classLoader.loadClass(toRuntimeClass(ascribe.tpe.symbol.syntax.init.substring("_root_.".length)))
    if (!clazz.isInstance(value)) {
      sys.error(s"Expected value of type ${ascribe.tpe}, but got $value")
    }
    (ref, newEnv)
  }

  def evalTuple(tupleTerm: Term.Tuple, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    var resEnv = env
    val values = for (expr <- tupleTerm.args) yield {
      val (ref, newEnv) = eval(expr, resEnv)
      resEnv = newEnv
      ref.reify(resEnv)
    }
    val tuple = values match {
      case Seq(t1) => Tuple1(t1)
      case Seq(t1, t2) => Tuple2(t1, t2)
      // TODO: code generate other cases as well
    }
    val ref = InterpreterJvmRef(null)
    (ref, resEnv.extend(ref, InterpreterWrappedJvm(tuple)))
  }

  def evalPatterns(
    toMatch: Seq[Any],
    patterns: Seq[Pat],
    env: Env
  )(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    var resEnv = env
    for ((value, pattern) <- toMatch.zip(patterns)) {
      val wrappedValue = InterpreterWrappedJvm(value) // FIXME: find intrpreter ref by value
      val valueRef = InterpreterJvmRef(null)
      resEnv = resEnv.extend(valueRef, wrappedValue)
      val (patRef, patEnv) = evalPattern(valueRef, pattern, resEnv)
      resEnv = patEnv
      if (!patRef.reifyBoolean(resEnv)) {
        return InterpreterRef.wrap(false, env, t"Boolean")
      }
    }
    InterpreterRef.wrap(true, resEnv, t"Boolean")
  }

  def evalPattern(
    toMatch: InterpreterRef,
    pat: Pat,
    env: Env
  )(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    pat match {
      case Pat.Alternative(lhs, rhs) =>
        val (patRef, patEnv) = evalPattern(toMatch, lhs, env)
        if (patRef.reifyBoolean(patEnv)) {
          (patRef, patEnv)
        } else {
          evalPattern(toMatch, rhs, env)
        }
      case Pat.Bind(lhs, rhs) =>
        val (patRef, patEnv) = evalPattern(toMatch, lhs, env)
        if (patRef.reifyBoolean(patEnv)) {
          (patRef, patEnv.extend(lhs.name.symbol, toMatch))
        } else {
          InterpreterRef.wrap(false, patEnv, t"Boolean")
        }
      case Pat.Extract(termRef, targs, args) =>
        ???
      case Pat.ExtractInfix(lhs, op, rhs) =>
        ???
      case Pat.Interpolate(prefix, parts, args) =>
        ???
      case literal: Lit =>
        val (litRef, litEnv) = evalLiteral(literal, env)
        InterpreterRef.wrap(
          litRef.reifyPrimitive(litEnv) == toMatch.reify(litEnv),
          litEnv,
          t"Boolean"
        )
      case name: Term.Name =>
        InterpreterRef.wrap(true, env.extend(name.symbol, toMatch), t"Boolean")
      case Term.Select(qual, name) =>
        ???
      case Pat.Var.Term(name) =>
        InterpreterRef.wrap(true, env.extend(name.symbol, toMatch), t"Boolean")
      case Pat.Tuple(args) =>
        Try(toMatch.reifyJvm(env)) match {
          case Failure(_) => InterpreterRef.wrap(false, env, t"Boolean")
          case Success(Tuple1(t1)) if args.size == 1 => evalPatterns(Seq(t1), args, env)
          case Success(Tuple2(t1, t2)) if args.size == 2 => evalPatterns(Seq(t1, t2), args, env)
          // TODO: code generate other cases as well
        }
      case Pat.Typed(lhs, rhs) =>
        val (resRef, resEnv) = evalPattern(toMatch, lhs, env)
        if (resRef.reifyBoolean(resEnv)) {
          toMatch.extract(resEnv) match {
            case InterpreterPrimitive(value) =>
              val classLoader = getClass.getClassLoader
              val clazz = classLoader.loadClass(toRuntimeClass(rhs.tpe.symbol.syntax.init.substring("_root_.".length)))
              InterpreterRef.wrap(clazz.isInstance(value), env, t"Boolean")
            case InterpreterWrappedJvm(value) =>
              val classLoader = getClass.getClassLoader
              val clazz = classLoader.loadClass(toRuntimeClass(rhs.tpe.symbol.syntax.init.substring("_root_.".length)))
              InterpreterRef.wrap(clazz.isInstance(value), env, t"Boolean")
            case InterpreterObject(classSymbol, fields) =>
              // TODO: check subtyping instead of equality
              InterpreterRef.wrap(classSymbol == rhs.tpe.symbol, env, t"Boolean")
          }
        } else {
          (resRef, resEnv)
        }
      case Pat.Wildcard() =>
        InterpreterRef.wrap(true, env, t"Boolean")
    }
  }

  def evalMatch(toMatchRef: InterpreterRef, termCases: Seq[Case], env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    for (termCase <- termCases) {
      val (evalRef, evalEnv) = evalPattern(toMatchRef, termCase.pat, env)
      if (evalRef.reifyBoolean(evalEnv)) {
        val (guardVal, guardEnv) = termCase.cond match {
          case Some(cond) =>
            val (condRef, condEnv) = eval(cond, evalEnv)
            (condRef.reifyBoolean(condEnv), condEnv)
          case None =>
            (true, evalEnv)
        }
        if (guardVal) {
          return eval(termCase.body, guardEnv)
        }
      }
    }
    sys.error("Match error")
  }

  def evalMatch(termMatch: Term.Match, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (toMatchRef, env1) = eval(termMatch.expr, env)
    evalMatch(toMatchRef, termMatch.cases, env1)
  }

  def evalTry(tryCatchTerm: Term.TryWithCases, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    try {
      eval(tryCatchTerm.expr, env)
    } catch {
      case InterpreterException(exceptionRef, exceptionEnv) =>
        evalMatch(exceptionRef, tryCatchTerm.catchp, exceptionEnv)
    }
  }

  def evalThrow(throwTerm: Term.Throw, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (throwRef, throwEnv) = eval(throwTerm.expr, env)
    throw InterpreterException(throwRef, throwEnv)
  }

  def evalReturn(returnTerm: Term.Return, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (retRef, retEnv) = eval(returnTerm.expr, env)
    throw ReturnException(retRef, retEnv)
  }

  def evalDoWhile(doTerm: Term.Do, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (_, env1) = eval(doTerm.body, env)
    evalWhile(Term.While(doTerm.expr, doTerm.body), env1)
  }

  def evalWhile(whileTerm: Term.While, env: Env)(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (condRef, env1) = eval(whileTerm.expr, env)
    val condRes = env1.heap(condRef)
    condRes match {
      case InterpreterPrimitive(value) => Try(value.asInstanceOf[Boolean]) match {
        case Success(bool) =>
          if (bool) {
            val (_, env2) = eval(whileTerm.body, env1)
            evalWhile(whileTerm, env2)
          } else {
            InterpreterRef.wrap((), env1, t"Unit")
          }
        case Failure(_) =>
          sys.error(s"Expected boolean, but got $value")
      }
      case _ =>
        sys.error(s"Expected boolean, but got $condRes")
    }
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
                  case Success(fun) =>
                    try {
                      fun(argRefs, env1.addThis(className, qualRef))
                    } catch {
                      case ReturnException(retRef, retEnv) => (retRef, retEnv)
                    }
                  case Failure(_) =>
                    sys.error(s"Tried to call ${name.value}, but it is not a function")
                }
              case None => sys.error(s"Unknown field $name for object $qualRef")
            }
          case _ => sys.error("Illegal state")
        }
      case name: Term.Name =>
        resEnv.stack.head.get(name.symbol) match {
          case Some(funRef: InterpreterFunctionRef) =>
            try {
              funRef(argRefs, resEnv)
            } catch {
              case ReturnException(retRef, retEnv) => (retRef, retEnv)
            }
          case Some(x)                              =>
            sys.error(s"Expected function, but got $x")
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
    case "<" => "testLessThan"
    case ">" => "testGreaterThan"
    case "<=" => "testLessOrEqualThan"
    case ">=" => "testGreaterOrEqualThan"
    case x   => x
  }

  def toRuntimeClass(name: String): String = name match {
    case "scala.Int"    => "java.lang.Integer"
    case "scala.Double" => "java.lang.Double"
    case x              => x
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
