package org.scalameta.interpreter.internal.environment

import org.scalameta.interpreter.internal.Engine

import scala.meta._

sealed trait InterpreterRef

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
}

case class InterpreterJvmRef(tpe: Type) extends InterpreterRef

object InterpreterRef {
  def wrap[T](value: T, env: Env, tpe: Type): (InterpreterRef, Env) = {
    val ref = InterpreterJvmRef(tpe)
    (ref, env.extend(ref, InterpreterPrimitive(value)))
  }
}
