package org.scalameta.interpreter.internal.environment

import org.scalameta.interpreter.internal.Engine

import scala.meta._

sealed trait InterpreterRef {
  def tpe: Type
}

case class InterpreterFunctionRef(
  params: Seq[Seq[Term.Param]],
  tparams: Seq[Type.Param],
  body: Tree,
  capturedEnv: Env
) extends InterpreterRef {
  def apply(argRefs: Seq[InterpreterRef], callSiteEnv: Env): (InterpreterRef, Env) = {
    val env1 = callSiteEnv.pushFrame(capturedEnv.stack.head)
    params match {
      case fParams :: Nil if fParams.size == argRefs.size =>
        val env2 = fParams
          .zip(argRefs)
          .foldLeft(env1)((tmpEnv, argRef) => tmpEnv.extend(argRef._1.name.value, argRef._2))
        val (res, env3) = Engine.eval(body, env2)
        (res, callSiteEnv.extendHeap(env3.heap))
      case fParams :: fParamss if fParams.size == argRefs.size =>
        val env2 = fParams
          .zip(argRefs)
          .foldLeft(env1)((tmpEnv, argRef) => tmpEnv.extend(argRef._1.name.value, argRef._2))
        (InterpreterFunctionRef(fParamss, tparams.tail, body, env2), callSiteEnv)
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

case class InterpreterCtorRef(
  className: ClassName,
  params: Seq[Seq[Term.Param]],
  tparams: Seq[Type.Param],
  body: Tree,
  capturedEnv: Env,
  parentConstructors: Seq[(InterpreterCtorRef, Seq[Term.Arg])]
) extends InterpreterRef {
  def apply(argRefs: Seq[InterpreterRef], callSiteEnv: Env): (InterpreterRef, Env) = {
    val env1 = callSiteEnv.pushFrame(capturedEnv.stack.head)
    params match {
      case fParams :: Nil if fParams.size == argRefs.size =>
        val env2 = fParams
          .zip(argRefs)
          .foldLeft(env1)((tmpEnv, argRef) => tmpEnv.extend(argRef._1.name.value, argRef._2))
        val ref       = InterpreterJvmRef(null)
        val obj       = InterpreterObject(className, env2.stack.head) // FIXME: not really env2.stack, but only ctor args
        val newStack = capturedEnv.stack
        val newHeap = env2.heap + (ref -> obj)
        val newThisContext = env2.thisContext + (className -> ref)
        val (_, env3) = Engine.eval(body, Env(newStack, newHeap, env2.classTable, newThisContext))
        val finalHeap   = callSiteEnv.heap ++ env3.heap
        (ref, Env(callSiteEnv.stack, finalHeap, callSiteEnv.classTable, callSiteEnv.thisContext))
      case fParams :: fParamss if fParams.size == argRefs.size =>
        sys.error("Can not process carried constructor")
      case fParams :: _ =>
        sys.error(s"Expected ${fParams.size} arguments for constructor, but got ${argRefs.size}")
      case Nil if argRefs.isEmpty =>
        val (_, env2) = Engine.eval(body, env1)
        val ref       = InterpreterJvmRef(null)
        val obj       = InterpreterObject(className, env2.stack.head)
        val newHeap   = callSiteEnv.heap ++ env2.heap + (ref -> obj)
        (ref, Env(callSiteEnv.stack, newHeap, callSiteEnv.classTable, callSiteEnv.thisContext))
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
}
