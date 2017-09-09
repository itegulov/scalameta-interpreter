package org.scalameta.interpreter.internal

import org.scalameta.interpreter.internal.environment._
import org.scalameta.interpreter._
import org.scalameta.interpreter.ScalametaMirror._

import scala.meta._
import cats._
import cats.data._
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._

import scala.reflect.NameTransformer
import scala.reflect.runtime.{universe => ru}

/**
 * @author Daniyar Itegulov
 */
class NewEngine(implicit mirror: ScalametaMirror) {
  type EnvState[A] = State[Env, A]
  type IState      = State[Env, InterpreterRef]

  implicit val r = MonadState[EnvState, Env]
  val m          = ru.runtimeMirror(getClass.getClassLoader)

  import r._

  def eval(tree: Tree): IState =
    tree match {
      case source: Source     => ???
      case definition: Defn   => ???
      case declaration: Decl  => wrap((), t"Unit") // FIXME: interpret them
      case importTerm: Import => wrap((), t"Unit") // Imports are already resolved by semantic API
      case template: Template => ???
      case term: Term         => evalTerm(term)
    }

  def evalTerm(term: Term): IState =
    term match {
      case apply: Term.Apply                 => ???
      case Term.ApplyInfix(lhs, op, _, xs)   => ???
      case Term.ApplyUnary(op, arg)          => ???
      case applyType: Term.ApplyType         => ???
      case ascribe: Term.Ascribe             => ???
      case assignment: Term.Assign           => ???
      case block: Term.Block                 => evalBlock(block)
      case doTerm: Term.Do                   => ???
      case eta: Term.Eta                     => ???
      case forTerm: Term.For                 => ???
      case forTerm: Term.ForYield            => ???
      case function: Term.Function           => ???
      case ifTerm: Term.If                   => evalIf(ifTerm)
      case interpolate: Term.Interpolate     => ???
      case literal: Lit                      => evalLit(literal)
      case termMatch: Term.Match             => ???
      case name: Term.Name                   => evalName(name)
      case newTerm: Term.New                 => ???
      case function: Term.PartialFunction    => ???
      case placeholder: Term.Placeholder     => ???
      case returnTerm: Term.Return           => ???
      case throwTerm: Term.Throw             => ???
      case tryCatchTerm: Term.Try            => ???
      case tryCatchTerm: Term.TryWithHandler => ???
      case tuple: Term.Tuple                 => ???
      case whileTerm: Term.While             => ???
      case select: Term.Select               => ???
      case xml: Term.Xml                     => sys.error("XMLs are unsupported")
    }

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

  def wrap[T](value: T, tpe: Type): IState =
    for {
      env <- get
      ref = InterpreterJvmRef(tpe)
      _ <- set(env.extend(ref, InterpreterPrimitive(value)))
    } yield ref

  def defaultValue(tpe: Type): IState = tpe match {
    case t"Byte"    => wrap(0.toByte, tpe)
    case t"Short"   => wrap(0.toShort, tpe)
    case t"Char"    => wrap(0.toChar, tpe)
    case t"Int"     => wrap(0, tpe)
    case t"Long"    => wrap(0l, tpe)
    case t"Float"   => wrap(0.0f, tpe)
    case t"Double"  => wrap(0.0d, tpe)
    case t"Boolean" => wrap(false, tpe)
    case t"Unit"    => wrap((), tpe)
    case _          => wrap(null, tpe)
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
