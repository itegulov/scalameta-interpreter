package org.scalameta.interpreter.internal.environment

import org.scalameta.interpreter.ScalametaMirror
import org.scalameta.interpreter.ScalametaMirror._
import org.scalameta.interpreter.internal.Engine

import scala.meta._

sealed trait InterpreterRef {
  def extract(env: Env): InterpreterValue =
    env.heap.get(this) match {
      case Some(value) => value
      case None        => sys.error("Illegal state")
    }

  def reify(env: Env): Any =
    env.heap.get(this) match {
      case Some(InterpreterPrimitive(value))  => value
      case Some(InterpreterWrappedJvm(value)) => value
      case Some(InterpreterObject(_, fields)) => InterpreterDynamic(fields)
      case None                               => sys.error("Illegal state")
    }

  def reifyJvm(env: Env): Any =
    env.heap.get(this) match {
      case Some(InterpreterWrappedJvm(value)) => value
      case Some(other)                        => sys.error(s"$other is not a JVM wrapped value")
      case None                               => sys.error("Illegal state")
    }

  def reifyPrimitive(env: Env): Any =
    env.heap.get(this) match {
      case Some(InterpreterPrimitive(prim))  => prim
      case Some(InterpreterWrappedJvm(prim)) => prim
      case Some(other)                       => sys.error(s"$other is not a primitive")
      case None                              => sys.error("Illegal state")
    }

  def reifyBoolean(env: Env): Boolean =
    reifyPrimitive(env) match {
      case true  => true
      case false => false
      case other => sys.error(s"Tried to reify $other as a boolean")
    }

  def tpe: Type
}

abstract class InterpreterFunctionRef extends InterpreterRef {
  def params: List[List[Term.Param]]
  def tparams: List[Type.Param]
  def body: Term
  def capturedEnv: Env
  def invoke(argRefs: List[InterpreterRef], callSiteEnv: Env): (InterpreterRef, Env)
}

final case class InterpreterDefinedPrefunctionRef(
  params: List[List[Term.Param]],
  tparams: List[Type.Param],
  body: Term,
  symbol: Symbol,
  capturedEnv: Env
)(implicit mirror: ScalametaMirror)
    extends InterpreterFunctionRef {
  override def invoke(argRefs: List[InterpreterRef], callSiteEnv: Env): (InterpreterRef, Env) =
    InterpreterDefinedFunctionRef(
      params,
      tparams,
      body,
      capturedEnv.extend(symbol, this)
    ).invoke(argRefs, callSiteEnv)

  override def tpe: Type = ???
}

final case class InterpreterDefinedFunctionRef(
  params: List[List[Term.Param]],
  tparams: List[Type.Param],
  body: Term,
  capturedEnv: Env
)(implicit mirror: ScalametaMirror)
    extends InterpreterFunctionRef {
  override def invoke(argRefs: List[InterpreterRef], callSiteEnv: Env): (InterpreterRef, Env) = {
    val env1 = callSiteEnv.pushFrame(capturedEnv.stack.head)
    params match {
      case fParams :: Nil
          if argRefs.size <= fParams.size && fParams
            .drop(argRefs.size)
            .forall(_.default.isDefined) =>
        var resEnv   = env1
        val defaults = fParams.drop(argRefs.size).map(_.default.get)
        val defaultRefs = for (default <- defaults) yield {
          val (res, newEnv) = Engine.eval(default, resEnv)
          resEnv = newEnv
          res
        }
        val env2 = fParams
          .zip(argRefs ++ defaultRefs)
          .foldLeft(resEnv)((tmpEnv, argRef) => tmpEnv.extend(argRef._1.name.symbol, argRef._2))
        val (res, env3) = Engine.eval(body, env2)
        (res, callSiteEnv.extendHeap(env3.heap))
      case fParams :: fParamss
          if argRefs.size <= fParams.size && fParams
            .drop(argRefs.size)
            .forall(_.default.isDefined) =>
        var resEnv   = env1
        val defaults = fParams.drop(argRefs.size).map(_.default.get)
        val defaultRefs = for (default <- defaults) yield {
          val (res, newEnv) = Engine.eval(default, resEnv)
          resEnv = newEnv
          res
        }
        val env2 = fParams
          .zip(argRefs ++ defaultRefs)
          .foldLeft(resEnv)((tmpEnv, argRef) => tmpEnv.extend(argRef._1.name.symbol, argRef._2))
        (InterpreterDefinedFunctionRef(fParamss, tparams.tail, body, env2), callSiteEnv)
      case fParams :: _ =>
        sys.error(s"Expected ${fParams.size} arguments, but got ${argRefs.size}")
      case Nil if argRefs.isEmpty =>
        val (res, env2) = Engine.eval(body, env1)
        (res, callSiteEnv.extendHeap(env2.heap))
      case Nil =>
        sys.error(s"Did not expect any arguments, but got ${argRefs.size}")
    }
  }

  override def tpe: Type = ???
}

abstract class InterpreterNativeFunctionRef extends InterpreterFunctionRef {
  override def tpe: Type = ???
}

final case class InterpreterCtorRef(
  className: Symbol,
  params: List[List[Term.Param]],
  tparams: List[Type.Param],
  body: Tree,
  capturedEnv: Env,
  parentConstructors: List[(InterpreterCtorRef, List[List[Term]])]
)(implicit mirror: ScalametaMirror)
    extends InterpreterRef {
  def applyRec(argRefss: List[List[InterpreterRef]], callSiteEnv: Env): (InterpreterRef, Env) =
    argRefss match {
      case Nil            => apply(Nil, callSiteEnv)
      case argRefs :: Nil => apply(argRefs, callSiteEnv)
      case argRefs :: tailRefs =>
        val (constructorRef, newEnv) = apply(argRefs, callSiteEnv)
        constructorRef.asInstanceOf[InterpreterCtorRef].applyRec(tailRefs, newEnv)
    }

  def apply(argRefs: List[InterpreterRef], callSiteEnv: Env): (InterpreterRef, Env) = {
    val env1 = callSiteEnv.pushFrame(capturedEnv.stack.head)
    params match {
      case fParams :: Nil if fParams.size == argRefs.size =>
        val env2 = fParams
          .zip(argRefs)
          .foldLeft(env1)((tmpEnv, argRef) => tmpEnv.extend(argRef._1.name.symbol, argRef._2))
        val parentObjectsAndEnvs = for ((parentConstructor, argss) <- parentConstructors) yield {
          val (ref, newEnv) = parentConstructor.applyRec(
            argss.map(_.map(_.asInstanceOf[Term.Name].symbol).map(env2.stack.head.apply)),
            env1
          )
          (newEnv.heap(ref).asInstanceOf[InterpreterObject], newEnv)
        }
        val (parentObjects, parentEnvs) = parentObjectsAndEnvs.unzip
        val ref                         = InterpreterJvmRef(null)
        val obj                         = InterpreterObject(className, env2.stack.head ++ parentObjects.flatMap(_.fields)) // FIXME: not really env2.stack, but only ctor args
        val newStack                    = capturedEnv.stack
        val newHeap                     = env2.heap + (ref -> obj) ++ parentEnvs.flatMap(_.heap)
        val newThisContext              = env2.thisContext + (className -> ref)
        val (_, env3)                   = Engine.eval(body, Env(newStack, newHeap, env2.classTable, newThisContext))
        val finalHeap                   = callSiteEnv.heap ++ env3.heap
        (
          ref,
          Env(
            callSiteEnv.stack,
            finalHeap,
            callSiteEnv.classTable,
            callSiteEnv.thisContext
          )
        )
      case fParams :: fParamss if fParams.size == argRefs.size =>
        sys.error("Can not process carried constructor")
      case fParams :: _ =>
        sys.error(s"Expected ${fParams.size} arguments for constructor, but got ${argRefs.size}")
      case Nil if argRefs.isEmpty =>
        val parentObjectsAndEnvs = for ((parentConstructor, args) <- parentConstructors) yield {
          val (ref, newEnv) = parentConstructor
            .apply(args.map(_.asInstanceOf[Term.Name].symbol).map(env1.stack.head.apply), env1)
          (newEnv.heap(ref).asInstanceOf[InterpreterObject], newEnv)
        }
        val (parentObjects, parentEnvs) = parentObjectsAndEnvs.unzip
        val ref                         = InterpreterJvmRef(null)
        val obj                         = InterpreterObject(className, env1.stack.head ++ parentObjects.flatMap(_.fields)) // FIXME: not really env1.stack, but only ctor args
        val newStack                    = capturedEnv.stack
        val newHeap                     = env1.heap + (ref -> obj) ++ parentEnvs.flatMap(_.heap)
        val newThisContext              = env1.thisContext + (className -> ref)
        val (_, env2)                   = Engine.eval(body, Env(newStack, newHeap, env1.classTable, newThisContext))
        val finalHeap                   = callSiteEnv.heap ++ env2.heap
        (
          ref,
          Env(
            callSiteEnv.stack,
            finalHeap,
            callSiteEnv.classTable,
            callSiteEnv.thisContext
          )
        )
      case Nil =>
        sys.error(s"Did not expect any arguments for constructor, but got ${argRefs.size}")
    }
  }

  override def tpe: Type = ???
}

class InterpreterJvmRef(val tpe: Type) extends InterpreterRef

object InterpreterJvmRef {
  def apply(tpe: Type): InterpreterJvmRef = new InterpreterJvmRef(tpe)
}

object InterpreterRef {
  def wrap[T](value: T, env: Env, tpe: Type): (InterpreterRef, Env) = {
    val ref = InterpreterJvmRef(tpe)
    (ref, env.extend(ref, InterpreterPrimitive(value)))
  }

  def wrapJvm[T](value: T, env: Env, tpe: Type): (InterpreterRef, Env) = {
    val ref = InterpreterJvmRef(tpe)
    (ref, env.extend(ref, InterpreterWrappedJvm(value)))
  }
}
