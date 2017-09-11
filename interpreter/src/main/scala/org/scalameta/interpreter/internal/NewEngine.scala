package org.scalameta.interpreter.internal

import java.lang.reflect.InvocationTargetException

import org.scalameta.interpreter.internal.environment._
import org.scalameta.interpreter._
import org.scalameta.interpreter.ScalametaMirror._

import scala.meta._
import cats._
import cats.data._
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._
import org.scalameta.interpreter.internal.flow.exceptions.{InterpreterException, ReturnException}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.NameTransformer
import scala.reflect.runtime.{universe => ru}
import scala.runtime.BoxesRunTime
import scala.util.{Failure, Success, Try}

object NewEngine {
  type EnvState[A] = State[Env, A]
  type IState      = State[Env, InterpreterRef]

  implicit val r = MonadState[EnvState, Env]
}

/**
 * @author Daniyar Itegulov
 */
class NewEngine(implicit mirror: ScalametaMirror) {
  import NewEngine._

  val m = ru.runtimeMirror(getClass.getClassLoader)

  import r._

  def emodify(f: Env => Env): IState =
    modify(f).flatMap { _ =>
      wrap((), t"Unit")
    }

  def eval(tree: Tree): IState =
    tree match {
      case source: Source     => evalSource(source)
      case definition: Defn   => evalDefn(definition)
      case declaration: Decl  => wrap((), t"Unit") // FIXME: interpret them
      case importTerm: Import => wrap((), t"Unit") // Imports are already resolved by semantic API
      case template: Template => evalTemplate(template)
      case term: Term         => evalTerm(term)
    }

  def evalTerm(term: Term): IState =
    term match {
      case apply: Term.Apply                   => evalApply(apply)
      case Term.ApplyInfix(lhs, op, _, xs)     => evalApply(Term.Apply(Term.Select(lhs, op), xs))
      case Term.ApplyUnary(op, arg)            => evalApply(Term.Apply(Term.Select(arg, op), List.empty))
      case applyType: Term.ApplyType           => ???
      case ascribe: Term.Ascribe               => evalAscribe(ascribe)
      case assignment: Term.Assign             => evalAssignment(assignment)
      case block: Term.Block                   => evalBlock(block)
      case doTerm: Term.Do                     => evalDoWhile(doTerm)
      case eta: Term.Eta                       => evalEta(eta)
      case forTerm: Term.For                   => evalFor(forTerm)
      case forYieldTerm: Term.ForYield         => evalForYield(forYieldTerm)
      case function: Term.Function             => evalFunction(function)
      case ifTerm: Term.If                     => evalIf(ifTerm)
      case interpolate: Term.Interpolate       => evalInterpolate(interpolate)
      case literal: Lit                        => evalLit(literal)
      case termMatch: Term.Match               => evalMatch(termMatch)
      case name: Term.Name                     => evalName(name)
      case newTerm: Term.New                   => evalNew(newTerm)
      case function: Term.PartialFunction      => evalPartialFunction(function)
      case placeholder: Term.Placeholder       => ???
      case returnTerm: Term.Return             => evalReturn(returnTerm)
      case throwTerm: Term.Throw               => evalThrow(throwTerm)
      case tryCatchTerm: Term.Try              => evalTry(tryCatchTerm)
      case tryHandlerTerm: Term.TryWithHandler => evalTryWithHandler(tryHandlerTerm)
      case tuple: Term.Tuple                   => evalTuple(tuple)
      case whileTerm: Term.While               => evalWhile(whileTerm)
      case select: Term.Select                 => evalSelect(select)
      case xml: Term.Xml                       => sys.error("XMLs are unsupported")
    }

  def evalSource(source: Source): IState =
    for {
      unit   <- wrap((), t"Unit")
      result <- source.stats.foldM[EnvState, InterpreterRef](unit) { case (_, stat) => eval(stat) }
    } yield result

  def evalDefn(definition: Defn): IState =
    definition match {
      case defnVal: Defn.Val       => evalDefnVal(defnVal)
      case defnVar: Defn.Var       => evalDefnVar(defnVar)
      case defnDef: Defn.Def       => evalDefnDef(defnDef)
      case defnTrait: Defn.Trait   => evalDefnTrait(defnTrait)
      case defnClass: Defn.Class   => evalDefnClass(defnClass)
      case defnMacro: Defn.Macro   => sys.error("Macroses are not supported")
      case defnObject: Defn.Object => evalDefnObject(defnObject)
      case defnType: Defn.Type     => wrap((), t"Unit") // Type aliases are already resolved by semantic API
    }

  def evalDefnVal(definition: Defn.Val): IState =
    for {
      rhsRef <- eval(definition.rhs)
      env    <- get
      result <- definition.parent match {
                 case Some(Template(_, _, _, _)) =>
                   val (_, ref) = env.thisContext.last
                   var resObj   = env.heap(ref).asInstanceOf[InterpreterObject]
                   for (pat <- definition.pats) {
                     pat match {
                       case Pat.Var(termName) =>
                         resObj = resObj.extend(termName.symbol, rhsRef)
                     }
                   }
                   emodify(_.extend(ref, resObj))
                 case _ =>
                   for {
                     _ <- definition.pats.map { pat =>
                           pat match {
                             case Pat.Var(name) =>
                               modify(_.extend(name.symbol, rhsRef))
                           }
                         }.sequence[EnvState, Unit]
                     result <- wrap((), t"Unit")
                   } yield result
               }
    } yield result

  def evalDefnVar(definition: Defn.Var): IState =
    (definition.decltpe, definition.rhs) match {
      case (_, Some(rhs)) =>
        evalDefnVal(
          Defn.Val(
            definition.mods,
            definition.pats,
            definition.decltpe,
            rhs
          )
        )
      case (Some(tpe), None) =>
        evalDefnVal(
          Defn.Val(
            definition.mods,
            definition.pats,
            definition.decltpe,
            defaultValue(tpe)
          )
        )
      case (None, None) => sys.error("Unreachable")
    }

  def evalDefnDef(definition: Defn.Def): IState =
    for {
      env <- get
      result <- definition.parent match {
                 case Some(Template(_, _, _, _)) =>
                   val (_, ref) = env.thisContext.last
                   val obj      = env.heap(ref).asInstanceOf[InterpreterObject]
                   val funRef = InterpreterDefinedPrefunctionRef(
                     definition.paramss,
                     definition.tparams,
                     definition.body,
                     definition.name.symbol,
                     env
                   )
                   val newObj = obj.extend(definition.name.symbol, funRef)
                   emodify(env => env.extend(definition.name.symbol, funRef).extend(ref, newObj))
                 case _ =>
                   val funRef = InterpreterDefinedPrefunctionRef(
                     definition.paramss,
                     definition.tparams,
                     definition.body,
                     definition.name.symbol,
                     env
                   )
                   emodify(_.extend(definition.name.symbol, funRef))
               }
    } yield result

  def evalDefnTrait(definition: Defn.Trait): IState = {
    require(definition.ctor.paramss.isEmpty, "Trait constructor should not have any parameters")
    ???
  }

  def evalDefnClass(definition: Defn.Class): IState =
    for {
      env <- get
      constructors = definition.templ.inits.map { init =>
        env.classTable.table.get(init.tpe.symbol) match {
          case Some(classInfo) => (classInfo.constructor, init.argss)
          case None            => sys.error(s"Unknown parent class: ${init.tpe}")
        }
      }
      ctorRef = InterpreterCtorRef(
        definition.name.symbol,
        definition.ctor.paramss,
        null,
        definition.templ,
        env,
        constructors
      )
      _      <- modify(_.addClass(definition.name.symbol, ClassInfo(ctorRef)))
      result <- wrap((), t"Unit")
    } yield result

  def evalDefnObject(definition: Defn.Object): IState =
    for {
      _   <- modify(_.pushFrame(Map.empty))
      _   <- eval(Term.Block(definition.templ.stats))
      env <- get
      obj = InterpreterObject(definition.name.symbol, env.stack.head)
      ref = InterpreterJvmRef(Type.Name(definition.name.value))
      _ <- modify { env =>
            Env(
              env.stack.tail,
              env.heap + (ref -> obj),
              env.classTable,
              env.thisContext
            )
          }
      _      <- modify(_.extend(definition.name.symbol, ref))
      result <- wrap((), t"Unit")
    } yield result

  def evalEta(eta: Term.Eta): IState =
    for {
      ref <- eval(eta.expr)
      result <- ref match {
                 case fun: InterpreterFunctionRef =>
                   fun.params match {
                     case xs :+ x =>
                       val f = xs.foldRight(Term.Function(x, fun.body)) {
                         case (params, function) => Term.Function(params, function)
                       }
                       evalFunction(f)
                     case Nil =>
                       evalFunction(Term.Function(List.empty, fun.body))
                   }
                 case other =>
                   inspect { env =>
                     new InterpreterNativeFunctionRef {
                       override def invoke(argRefs: List[InterpreterRef],
                                           callSiteEnv: Env): (InterpreterRef, Env) = {
                         if (argRefs.nonEmpty) {
                           sys.error("Did not expect any arguments for this function")
                         }
                         (other, callSiteEnv)
                       }

                       override def params: List[List[Term.Param]] = List.empty

                       override def tparams: List[Type.Param] = List.empty

                       override def body: Term = sys.error("Illegal state")

                       override def capturedEnv: Env = env
                     }
                   }
               }
    } yield result

  def evalInterpolate(interpolate: Term.Interpolate): IState =
    eval(q"new StringContext(..${interpolate.parts}).${interpolate.prefix}(..${interpolate.args})")

  def evalPartialFunction(partialFunction: Term.PartialFunction): IState = {
    // TODO: replace this temporal name hack
    val x         = Term.Name("__interpreterMatchX__")
    val xParam    = Term.Param(List.empty[Mod], x, None, None)
    val termMatch = Term.Match(x, partialFunction.cases)
    inspect { env =>
      InterpreterDefinedFunctionRef(
        List(List(xParam)),
        null,
        termMatch,
        env
      )
    }
  }

  def evalFunction(function: Term.Function): IState =
    inspect { env =>
      InterpreterDefinedFunctionRef(
        List(function.params),
        null,
        function.body,
        env
      )
    }

  def evalForYield(forTerm: Term.ForYield): IState = {

    case object NoValue

    def evalForYieldRec(enums: Seq[Enumerator]): IState =
      enums match {
        case Enumerator.Generator(pat, rhs) :: tail =>
          for {
            rhsRef <- eval(rhs)
            rhsVal <- rhsRef.reify
            result <- Try(rhsVal.asInstanceOf[Iterable[Any]]) match {
                       case Success(iterableRhs) =>
                         pat match {
                           case Pat.Var(name) =>
                             val resultBufferRef = InterpreterJvmRef(null)
                             val resultBuffer    = ArrayBuffer.empty[Any]
                             for {
                               _ <- iterableRhs.map { oneRhs =>
                                     val oneRhsRef       = InterpreterJvmRef(null)
                                     val resultBufferRef = InterpreterJvmRef(null)
                                     for {
                                       _ <- modify(
                                             _.extend(oneRhsRef, InterpreterWrappedJvm(oneRhs))
                                           )
                                       newRef <- evalForYieldRec(tail)
                                       newVal <- newRef.reify
                                       _ = newVal match {
                                         case NoValue               =>
                                         case iterable: Iterable[_] => resultBuffer ++= iterable
                                         case other                 => resultBuffer += other
                                       }
                                     } yield newRef
                                   }.toList.sequence[EnvState, InterpreterRef]
                               result <- wrapJvm(resultBuffer, null)
                             } yield result
                         }
                       case Failure(_) => sys.error(s"Expected iterable in for, but got $rhsVal")
                     }
          } yield result
        case Enumerator.Guard(cond) :: tail =>
          for {
            condRef <- eval(cond)
            condVal <- condRef.reifyBoolean
            result <- if (condVal) {
                       evalForYieldRec(tail)
                     } else {
                       wrapJvm(NoValue, null)
                     }
          } yield result
        case Enumerator.Val(pat, rhs) :: tail =>
          for {
            rhsRef <- eval(rhs)
            result <- pat match {
                       case Pat.Var(name) =>
                         modify(_.extend(name.symbol, rhsRef)).flatMap(_ => evalForYieldRec(tail))
                     }
          } yield result
        case Nil =>
          eval(forTerm.body).flatMap(_ => wrap((), t"Unit"))
      }
    evalForYieldRec(forTerm.enums)
  }

  def evalFor(forTerm: Term.For): IState = {
    def evalForRec(enums: Seq[Enumerator]): IState =
      enums match {
        case Enumerator.Generator(pat, rhs) :: tail =>
          for {
            rhsRef <- eval(rhs)
            rhsVal <- rhsRef.reify
            _ <- Try(rhsVal.asInstanceOf[Iterable[Any]]) match {
                  case Success(iterableRhs) =>
                    pat match {
                      case Pat.Var(name) =>
                        iterableRhs.map { oneRhs =>
                          val oneRhsRef = InterpreterJvmRef(null)
                          for {
                            _      <- modify(_.extend(oneRhsRef, InterpreterWrappedJvm(oneRhs)))
                            _      <- modify(_.extend(name.symbol, oneRhsRef))
                            result <- evalForRec(tail)
                          } yield result
                        }.toList.sequence[EnvState, InterpreterRef]
                    }
                  case Failure(_) => sys.error(s"Expected iterable in for, but got $rhsVal")
                }
            result <- wrap((), t"Unit")
          } yield result
        case Enumerator.Guard(cond) :: tail =>
          for {
            condRef <- eval(cond)
            condVal <- condRef.reifyBoolean
            result <- if (condVal) {
                       evalForRec(tail)
                     } else {
                       wrap((), t"Unit")
                     }
          } yield result
        case Enumerator.Val(pat, rhs) :: tail =>
          for {
            rhsRef <- eval(rhs)
            result <- pat match {
                       case Pat.Var(name) =>
                         modify(_.extend(name.symbol, rhsRef)).flatMap(_ => evalForRec(tail))
                     }
          } yield result
        case Nil =>
          eval(forTerm.body).flatMap(_ => wrap((), t"Unit"))
      }
    evalForRec(forTerm.enums)
  }

  def evalAscribe(ascribe: Term.Ascribe): IState =
    for {
      ref   <- eval(ascribe.expr)
      value <- ref.reify
      classLoader = getClass.getClassLoader
      clazz = classLoader.loadClass(
        toRuntimeClass(ascribe.tpe.symbol.syntax.init.substring("_root_.".length))
      )
      result <- if (!clazz.isInstance(value)) {
                 sys.error(s"Expected value of type ${ascribe.tpe}, but got $value")
               } else {
                 ref.pure[EnvState]
               }
    } yield result

  def evalTuple(tupleTerm: Term.Tuple): IState =
    for {
      values <- tupleTerm.args.map(eval).map(_.flatMap(_.reify)).sequence[EnvState, Any]
      tuple = values match {
        case Seq(t1)     => Tuple1(t1)
        case Seq(t1, t2) => Tuple2(t1, t2)
        // TODO: code generate other cases as well
      }
      ref = InterpreterJvmRef(null)
      _ <- modify(_.extend(ref, InterpreterWrappedJvm(tuple)))
    } yield ref

  def evalPatterns(toMatch: Seq[Any], patterns: Seq[Pat]): IState =
    for {
      matches <- toMatch
                  .zip(patterns)
                  .toList
                  .map {
                    case (value, pattern) =>
                      val wrappedValue = InterpreterWrappedJvm(value) // FIXME: find intrpreter ref by value
                      val valueRef     = InterpreterJvmRef(null)
                      for {
                        _      <- modify(_.extend(valueRef, wrappedValue))
                        patRef <- evalPattern(valueRef, pattern)
                        patVal <- patRef.reifyBoolean
                      } yield patVal
                  }
                  .sequence[EnvState, Boolean]
      result <- if (matches.forall(identity)) wrap(true, t"Boolean") else wrap(false, t"Boolean")
    } yield result

  def evalPattern(toMatch: InterpreterRef, pat: Pat): IState =
    pat match {
      case Pat.Alternative(lhs, rhs) =>
        for {
          patRef <- evalPattern(toMatch, lhs)
          patVal <- patRef.reifyBoolean
          result <- if (patVal) patRef.pure[EnvState] else evalPattern(toMatch, rhs)
        } yield result
      case Pat.Bind(lhs: Pat.Var, rhs) =>
        for {
          patRef <- evalPattern(toMatch, lhs)
          patVal <- patRef.reifyBoolean
          result <- if (patVal)
                     modify(_.extend(lhs.name.symbol, toMatch)).flatMap(_ => patRef.pure[EnvState])
                   else wrap(false, t"Boolean")
        } yield result
      case Pat.Extract(fun, args) =>
        fun match {
          case name: Term.Name =>
            metaToReflect(name.symbol, m) match {
              case (_, module: ru.ModuleSymbol) =>
                for {
                  value <- toMatch.reify
                  im                   = m.reflectModule(module)
                  moduleInstanceMirror = m.reflect(im.instance)
                  result <- if (module.typeSignature.member(ru.TermName("unapplySeq")) != ru.NoSymbol) {
                             val method = module.typeSignature.member(ru.TermName("unapplySeq"))
                             val unapplyOptResult =
                               moduleInstanceMirror.reflectMethod(method.asMethod)(value)
                             unapplyOptResult match {
                               case Some(unapplyResult: Seq[_])
                                   if unapplyResult.length == args.length =>
                                 evalPatterns(unapplyResult, args)
                               case Some(_) | None => wrap(false, t"Boolean")
                             }
                           } else {
                             val method = module.typeSignature.member(ru.TermName("unapply"))
                             val unapplyOptResult =
                               moduleInstanceMirror.reflectMethod(method.asMethod)(value)
                             unapplyOptResult match {
                               case Some(unapplyResult) =>
                                 unapplyResult match {
                                   case Tuple1(v1)     => evalPatterns(Seq(v1), args)
                                   case Tuple2(v1, v2) => evalPatterns(Seq(v1, v2), args)
                                   // TODO: code generate other cases as well
                                   case v if args.length == 1 => evalPatterns(Seq(v), args)
                                   case _ =>
                                     sys.error("Expected tuple or raw value for one argument")
                                 }
                               case None => wrap(false, t"Boolean")
                               case _ =>
                                 sys.error(
                                   s"Expected Option[T] from `unapply` invocation, but got $unapplyOptResult"
                                 )
                             }
                           }
                } yield result
            }
        }
      case Pat.ExtractInfix(lhs, op, rhs) =>
        evalPattern(toMatch, Pat.Extract(op, lhs +: rhs))
      case Pat.Interpolate(prefix, parts, args) =>
        ???
      case literal: Lit =>
        for {
          litRef     <- evalLit(literal)
          litVal     <- litRef.reifyPrimitive
          toMatchVal <- toMatch.reify
          result     <- wrap(litVal == toMatchVal, t"Boolean")
        } yield result
      case name: Term.Name =>
        if (name.symbol.isObject) {
          val m           = ru.runtimeMirror(getClass.getClassLoader)
          val (_, symbol) = metaToReflect(name.symbol, m)
          val module      = symbol.asModule
          val im          = m.reflectModule(module)
          for {
            toMatchVal <- toMatch.reify
            result     <- wrap(im.instance == toMatchVal, t"Boolean")
          } yield result
        } else {
          for {
            _      <- modify(_.extend(name.symbol, toMatch))
            result <- wrap(true, t"Boolean")
          } yield result
        }
      case Term.Select(qual, name) =>
        ???
      case Pat.Var(name) =>
        for {
          _      <- modify(_.extend(name.symbol, toMatch))
          result <- wrap(true, t"Boolean")
        } yield result
      case Pat.Tuple(args) =>
        for {
          toMatchVal <- Try(toMatch.reifyJvm).sequence[EnvState, Any]
          result <- toMatchVal match {
                     case Failure(_)                            => wrap(false, t"Boolean")
                     case Success(Tuple1(t1)) if args.size == 1 => evalPatterns(Seq(t1), args)
                     case Success(Tuple2(t1, t2)) if args.size == 2 =>
                       evalPatterns(Seq(t1, t2), args)
                     // TODO: code generate other cases as well
                   }
        } yield result
      case Pat.Typed(lhs, rhs) =>
        for {
          resRef <- evalPattern(toMatch, lhs)
          resVal <- resRef.reifyBoolean
          result <- if (resVal) {
                     for {
                       toMatchVal <- toMatch.extract
                       result <- toMatchVal match {
                                  case InterpreterPrimitive(value) =>
                                    val classLoader = getClass.getClassLoader
                                    val clazz = classLoader.loadClass(
                                      toRuntimeClass(
                                        rhs.symbol.syntax.init.substring("_root_.".length)
                                      )
                                    )
                                    wrap(clazz.isInstance(value), t"Boolean")
                                  case InterpreterWrappedJvm(value) =>
                                    val classLoader = getClass.getClassLoader
                                    val clazz = classLoader.loadClass(
                                      toRuntimeClass(
                                        rhs.symbol.syntax.init.substring("_root_.".length)
                                      )
                                    )
                                    wrap(clazz.isInstance(value), t"Boolean")
                                  case InterpreterObject(classSymbol, fields) =>
                                    // TODO: check subtyping instead of equality
                                    wrap(classSymbol == rhs.symbol, t"Boolean")
                                }
                     } yield result
                   } else {
                     resRef.pure[EnvState]
                   }
        } yield result
      case Pat.Wildcard() =>
        wrap(true, t"Boolean")
    }

  def evalMatch(toMatchRef: InterpreterRef, termCases: Seq[Case]): IState =
    for {
      matches <- termCases.toList.map { termCase =>
                  for {
                    evalRef   <- evalPattern(toMatchRef, termCase.pat)
                    evalValue <- evalRef.reifyBoolean
                    result <- if (evalValue) {
                               termCase.cond match {
                                 case Some(cond) => eval(cond).flatMap(_.reifyBoolean)
                                 case None       => true.pure[EnvState]
                               }
                             } else {
                               false.pure[EnvState]
                             }
                  } yield (termCase, result)
                }.sequence[EnvState, (Case, Boolean)]
      resultTermCase = matches.find(_._2).map(_._1).getOrElse(sys.error("Match error"))
      result <- eval(resultTermCase)
    } yield result

  def evalMatch(termMatch: Term.Match): IState =
    for {
      toMatchRef <- eval(termMatch.expr)
      result     <- evalMatch(toMatchRef, termMatch.cases)
    } yield result

  def evalTryWithHandler(tryCatchTerm: Term.TryWithHandler): IState =
    for {
      tryRef <- try {
                 eval(tryCatchTerm.expr)
               } catch {
                 case InterpreterException(_, env) =>
                   for {
                     _      <- set(env)
                     result <- eval(tryCatchTerm.catchp)
                   } yield result
               }
      _ <- tryCatchTerm.finallyp.map(eval).sequence[EnvState, InterpreterRef]
    } yield tryRef

  def evalTry(tryCatchTerm: Term.Try): IState =
    for {
      tryRef <- try {
                 eval(tryCatchTerm.expr)
               } catch {
                 case InterpreterException(exceptionRef, exceptionEnv) =>
                   evalMatch(exceptionRef, tryCatchTerm.catchp)
               }
      _ <- tryCatchTerm.finallyp.map(eval).sequence[EnvState, InterpreterRef]
    } yield tryRef

  def evalThrow(throwTerm: Term.Throw): IState =
    for {
      throwRef <- eval(throwTerm.expr)
      env      <- get
    } yield throw InterpreterException(throwRef, env)

  def evalReturn(returnTerm: Term.Return): IState =
    for {
      retRef <- eval(returnTerm.expr)
      env    <- get
    } yield throw ReturnException(retRef, env)

  def evalDoWhile(doTerm: Term.Do): IState =
    for {
      _      <- eval(doTerm.body)
      result <- evalWhile(Term.While(doTerm.expr, doTerm.body))
    } yield result

  def evalWhile(whileTerm: Term.While): IState =
    for {
      condRef   <- eval(whileTerm.expr)
      condValue <- condRef.reifyBoolean
      result <- if (condValue) {
                 eval(whileTerm.body).flatMap(_ => eval(whileTerm))
               } else {
                 wrap((), t"Unit")
               }
    } yield result

  def evalTemplate(template: Template): IState =
    for {
      _      <- template.stats.map(eval).sequence
      result <- wrap((), t"Unit")
    } yield result

  def evalSelectValue(value: Any, name: Term.Name): IState = {
    val mirror              = m.reflect(value)
    val symbol              = mirror.symbol.typeSignature.member(ru.TermName(name.value))
    val result              = mirror.reflectMethod(symbol.asMethod)()
    val ref: InterpreterRef = InterpreterJvmRef(null)
    modify(_.extend(ref, InterpreterPrimitive(result))).flatMap(_ => ref.pure[EnvState])
  }

  def evalSelect(select: Term.Select): IState =
    for {
      qualRef <- eval(select.qual)
      env     <- get
      result <- env.heap.get(qualRef) match {
                 case Some(InterpreterPrimitive(value)) =>
                   evalSelectValue(value, select.name)
                 case Some(InterpreterObject(_, fields)) =>
                   fields
                     .get(select.name.symbol)
                     .map(_.pure[EnvState])
                     .getOrElse(sys.error(s"Unknown field ${select.name} for object $qualRef"))
                 case Some(InterpreterWrappedJvm(value)) =>
                   evalSelectValue(value, select.name)
                 case None =>
                   sys.error("Illegal state")
               }
    } yield result

  def evalNew(newTerm: Term.New): IState =
    for {
      argRefs <- newTerm.init.argss
                  .map(args => args.map(eval).sequence[EnvState, InterpreterRef])
                  .sequence[EnvState, List[InterpreterRef]]
      env <- get
      result <- env.classTable.table.get(newTerm.init.tpe.symbol) match {
                 case Some(classInfo) =>
                   classInfo.constructor.applyRec(argRefs)
                 case None =>
                   val (_, symbol) = metaToReflect(newTerm.init.tpe.symbol, m)
                   val module      = symbol.asModule
                   for {
                     argValues <- argRefs.flatten.map(_.reify).sequence[EnvState, Any]
                     moduleMirror = m.reflectModule(module)
                     classMirror  = m.reflectClass(moduleMirror.symbol.companion.asClass)
                     constructor = classMirror.symbol.info
                       .member(ru.termNames.CONSTRUCTOR)
                       .asMethod
                     constructorMirror = classMirror.reflectConstructor(constructor)
                     wrappedJvm        = InterpreterWrappedJvm(constructorMirror.apply(argValues))
                     jvmRef            = InterpreterJvmRef(null)
                     _ <- modify(_.extend(jvmRef, wrappedJvm))
                   } yield jvmRef
               }
    } yield result

  def evalAssignment(assignment: Term.Assign): IState =
    for {
      assignmentRef <- eval(assignment.rhs)
      result <- assignment.lhs match {
                 case name: Term.Name =>
                   emodify(_.extend(name.symbol, assignmentRef))
                 case Term.Select(qual, name) =>
                   for {
                     ref <- eval(qual)
                     env <- get
                     result <- env.heap.get(ref) match {
                                case Some(InterpreterPrimitive(value)) =>
                                  sys.error("Can not mutate primitives")
                                case Some(obj @ InterpreterObject(_, fields))
                                    if fields.contains(name.symbol) =>
                                  val newObj = obj.extend(name.symbol, assignmentRef)
                                  emodify(_.extend(ref, newObj))
                                case Some(InterpreterObject(_, _)) =>
                                  sys.error(s"Unknown field ${name.value} for $ref")
                                case None => sys.error("Illegal state")
                              }
                   } yield result
                 case Term.Apply(fun, args) =>
                   evalApply(
                     Term.Apply(Term.Select(fun, Term.Name("update")), args :+ assignment.rhs)
                   )
                 case _ =>
                   sys.error(s"Can not interpret unrecognized tree ${assignment.lhs}")
               }
    } yield result

  def evalApplySelectJvm(value: Any, name: Term.Name, argRefs: List[InterpreterRef]): IState =
    for {
      argValues <- argRefs.map(_.reify).sequence[EnvState, Any]
      result <- name.symbol match {
                 case ScalametaMirror.AnyEquals =>
                   argValues match {
                     case Seq(arg) => wrap(value == arg, t"Boolean")
                     case _        => sys.error(s"Expected one argument for equals(), but got $argValues")
                   }
                 case ScalametaMirror.AnyHashcode =>
                   argValues match {
                     case Seq() => wrap(value.hashCode(), t"Int")
                     case _     => sys.error(s"Expected no arguments for hashCode, but got $argValues")
                   }
                 case ScalametaMirror.`Any==` =>
                   argValues match {
                     case Seq(arg) => wrap(value == arg, t"Boolean")
                     case _        => sys.error(s"Expected one argument for ==, but got $argValues")
                   }
                 case ScalametaMirror.`Any!=` =>
                   argValues match {
                     case Seq(arg) => wrap(value != arg, t"Boolean")
                     case _        => sys.error(s"Expected one argument for !=, but got $argValues")
                   }
                 case _ =>
                   val runtimeName = toRuntimeName(name.value)
                   val allFuns =
                     classOf[BoxesRunTime].getDeclaredMethods.filter(_.getName == runtimeName)
                   try {
                     val fun = allFuns.head
                     val result =
                       fun.invoke(null, (value +: argValues).asInstanceOf[Seq[AnyRef]]: _*)
                     val ref = InterpreterJvmRef(null)
                     State[Env, InterpreterRef] { env =>
                       (env.extend(ref, InterpreterPrimitive(result)), ref)
                     }
                   } catch {
                     case _: InvocationTargetException | _: NoSuchElementException =>
                       val m  = ru.runtimeMirror(getClass.getClassLoader)
                       val im = m.reflect(value)
                       val method = im.symbol.typeSignature
                         .member(ru.TermName(NameTransformer.encode(name.value)))
                       val methodMirror = im.reflectMethod(method.asMethod)
                       val newValue = if (method.asMethod.isVarargs) {
                         InterpreterWrappedJvm(methodMirror(argValues))
                       } else {
                         InterpreterWrappedJvm(methodMirror(argValues: _*))
                       }
                       val newRef = InterpreterJvmRef(null)
                       State[Env, InterpreterRef] { env =>
                         (env.extend(newRef, newValue), newRef)
                       }
                   }
               }
    } yield result

  def evalApplySelectObject(
    qualRef: InterpreterRef,
    classSymbol: Symbol,
    fields: Map[Symbol, InterpreterRef],
    name: Term.Name,
    argRefs: List[InterpreterRef]
  ): IState =
    fields.get(name.symbol) match {
      case Some(fun: InterpreterFunctionRef) =>
        try {
          State { env =>
            val (ref, newEnv) = fun.invoke(argRefs, env.addThis(classSymbol, qualRef))
            (newEnv, ref)
          }
        } catch {
          case ReturnException(retRef, retEnv) => State(_ => (retEnv, retRef))
        }
      case Some(_) => sys.error("Invalid state")
      case None    => sys.error(s"Unknown field ${name.value} for object $classSymbol")
    }

  def evalApplySelect(qual: Term, name: Term.Name, argRefs: List[InterpreterRef]): IState =
    for {
      qualRef <- eval(qual)
      qualVal <- qualRef.extract
      result <- qualVal match {
                 case InterpreterPrimitive(value) =>
                   evalApplySelectJvm(value, name, argRefs)
                 case InterpreterWrappedJvm(value) =>
                   evalApplySelectJvm(value, name, argRefs)
                 case InterpreterObject(classSymbol, fields) =>
                   evalApplySelectObject(qualRef, classSymbol, fields, name, argRefs)
               }
    } yield result

  def evalApplyName(name: Term.Name, argRefs: List[InterpreterRef]): IState =
    for {
      x <- State.inspect[Env, Option[InterpreterRef]](_.getLocal(name.symbol))
      y <- x match {
            case Some(funRef: InterpreterFunctionRef) =>
              try {
                funRef.invokeNew(argRefs)
              } catch {
                case ReturnException(retRef, retEnv) =>
                  State[Env, InterpreterRef](_ => (retEnv, retRef))
              }
            case Some(ref) =>
              ref.extract.flatMap[InterpreterRef] {
                case InterpreterPrimitive(value) =>
                  sys.error(s"Expected function, but got $value")
                case InterpreterObject(_, fields) =>
                  fields.get(Term.Name("apply").symbol) match {
                    case Some(funRef: InterpreterFunctionRef) =>
                      try {
                        funRef.invokeNew(argRefs)
                      } catch {
                        case ReturnException(retRef, retEnv) => State(_ => (retEnv, retRef))
                      }
                    case _ => sys.error(s"There is no method 'apply' for $ref")
                  }
                case InterpreterWrappedJvm(jvmValue) =>
                  for {
                    argValues <- argRefs.map(_.reify).sequence[EnvState, Any]
                    im     = m.reflect(jvmValue)
                    method = im.symbol.typeSignature.member(ru.TermName("apply"))
                    newValue = InterpreterWrappedJvm(
                      im.reflectMethod(method.asMethod)(argValues: _*)
                    )
                    newRef: InterpreterRef = InterpreterJvmRef(null)
                    _ <- modify(_.extend(newRef, newValue))
                  } yield newRef
              }
            case None =>
              for {
                argValues <- argRefs.map(_.reify).sequence[EnvState, Any]
                result <- metaToReflect(name.symbol, m) match {
                           case (owner: ru.ModuleSymbol, method: ru.MethodSymbol) =>
                             val im                 = m.reflectModule(owner)
                             val objScalametaMirror = m.reflect(im.instance)
                             val ref                = InterpreterJvmRef(null)

                             val res = if (method.isVarargs) {
                               InterpreterWrappedJvm(
                                 objScalametaMirror.reflectMethod(method)(argValues)
                               )
                             } else {
                               InterpreterWrappedJvm(
                                 objScalametaMirror.reflectMethod(method)(argValues: _*)
                               )
                             }
                             for {
                               _ <- modify(_.extend(ref, res))
                             } yield ref
                           case (_, module: ru.ModuleSymbol) =>
                             val im                 = m.reflectModule(module)
                             val objScalametaMirror = m.reflect(im.instance)
                             val ref                = InterpreterJvmRef(null)

                             val alternatives = im.symbol.info
                               .member(ru.TermName("apply"))
                               .asTerm
                               .alternatives
                               .map(_.asMethod)
                             val method = alternatives.head
                             val res = if (method.isVarargs) {
                               InterpreterWrappedJvm(
                                 objScalametaMirror.reflectMethod(method)(argValues)
                               )
                             } else {
                               InterpreterWrappedJvm(
                                 objScalametaMirror.reflectMethod(method)(argValues: _*)
                               )
                             }
                             for {
                               _ <- modify(_.extend(ref, res))
                             } yield ref
                         }
              } yield result
          }
    } yield y

  def evalApply(apply: Term.Apply): IState =
    for {
      argRefs <- apply.args.map(eval).sequence[EnvState, InterpreterRef]
      result <- apply.fun match {
                 case Term.Select(qual, name) => evalApplySelect(qual, name, argRefs)
                 case name: Term.Name         => evalApplyName(name, argRefs)
               }
    } yield result

  def evalIf(ifTerm: Term.If): IState =
    for {
      condRef   <- eval(ifTerm.cond)
      condition <- condRef.reifyBoolean
      res <- if (condition) {
              eval(ifTerm.thenp)
            } else {
              eval(ifTerm.elsep)
            }
    } yield res

  def evalName(name: Term.Name): IState =
    if (name.symbol.isObject) {
      reflectFromObject(name.symbol)
    } else {
      State { env =>
        val ref = env.getLocal(name.symbol).getOrElse(sys.error(s"Unknown reference $name"))
        (env, ref)
      }
    }

  def evalBlock(block: Term.Block): IState =
    for {
      unit   <- wrap((), t"Unit")
      result <- block.stats.foldM[EnvState, InterpreterRef](unit) { case (_, stat) => eval(stat) }
    } yield result

  def evalLit(literal: Lit): IState = literal match {
    case Lit.Byte(value)    => wrap(value, t"Byte")
    case Lit.Short(value)   => wrap(value, t"Short")
    case Lit.Char(value)    => wrap(value, t"Char")
    case Lit.Int(value)     => wrap(value, t"Int")
    case Lit.Long(value)    => wrap(value, t"Long")
    case Lit.Float(value)   => wrap(value.toFloat, t"Float")
    case Lit.Double(value)  => wrap(value.toDouble, t"Double")
    case Lit.Boolean(value) => wrap(value, t"Boolean")
    case Lit.Unit()         => wrap((), t"Unit")
    case Lit.String(value)  => wrap(value, t"String")
    case Lit.Null()         => wrap(null, t"Any")
    case Lit.Symbol(_)      => sys.error(s"Can not interpret symbol tree $literal")
    case _                  => sys.error(s"Can not interpret unrecognized tree $literal")
  }

  def reflectFromObject(nameSymbol: Symbol): IState = State { env =>
    val (_, symbol) = metaToReflect(nameSymbol, m)
    val module      = symbol.asModule
    val im          = m.reflectModule(module)
    val jvmValue    = InterpreterWrappedJvm(im.instance)
    val jvmRef      = InterpreterJvmRef(null)
    (env.extend(jvmRef, jvmValue), jvmRef)
  }

  def wrapJvm[T](value: T, tpe: Type): IState = {
    val ref: InterpreterRef = InterpreterJvmRef(tpe)
    modify(_.extend(ref, InterpreterWrappedJvm(value))).flatMap(_ => ref.pure[EnvState])
  }

  def wrap[T](value: T, tpe: Type): IState = {
    val ref: InterpreterRef = InterpreterJvmRef(tpe)
    modify(_.extend(ref, InterpreterPrimitive(value))).flatMap(_ => ref.pure[EnvState])
  }

  def defaultValue(tpe: Type): Lit = tpe match {
    case t"Byte"    => Lit.Byte(0.toByte)
    case t"Short"   => Lit.Short(0.toShort)
    case t"Char"    => Lit.Char(0.toChar)
    case t"Int"     => Lit.Int(0)
    case t"Long"    => Lit.Long(0l)
    case t"Float"   => Lit.Float(0f)
    case t"Double"  => Lit.Double(0d)
    case t"Boolean" => Lit.Boolean(false)
    case t"Unit"    => Lit.Unit()
    case _          => Lit.Null()
  }

  def toRuntimeName(name: String): String = name match {
    case "&&" => "takeConditionalAnd"
    case "||" => "takeConditionalOr"
    case "!"  => "takeNot"
    case "+"  => "add"
    case "-"  => "subtract"
    case "*"  => "multiply"
    case "/"  => "divide"
    case "<"  => "testLessThan"
    case ">"  => "testGreaterThan"
    case "<=" => "testLessOrEqualThan"
    case ">=" => "testGreaterOrEqualThan"
    case x    => x
  }

  def toRuntimeClass(name: String): String = name match {
    case "scala.Int"    => "java.lang.Integer"
    case "scala.Double" => "java.lang.Double"
    case x              => x
  }

  def metaToReflect(s: Symbol, m: ru.Mirror): (ru.Symbol, ru.Symbol) = s match {
    case Symbol.Global(Symbol.None, Signature.Term("_root_")) =>
      (null, m.staticPackage("_root_"))
    case Symbol.Global(owner, Signature.Term(termName)) =>
      val ownerModule = m.staticPackage(owner.syntax.init)
      val im          = m.reflectModule(ownerModule)
      (ownerModule, im.symbol.info.member(ru.TermName(NameTransformer.encode(termName))))
    case Symbol.Global(owner, Signature.Method(methodName, jvmSignature)) =>
      val ownerModule = m.staticModule(owner.syntax.init)
      val im          = m.reflectModule(ownerModule)
      val alternatives =
        im.symbol.info
          .member(ru.TermName(NameTransformer.encode(methodName)))
          .asTerm
          .alternatives
          .map(_.asMethod)
      //          .filter(_.paramLists.head.size == argRefs.size) // FIXME: add proper argument type check
      (ownerModule, alternatives.head)
  }
}
