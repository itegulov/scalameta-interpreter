package org.scalameta.interpreter.internal.environment

import scala.meta._

sealed trait InterpreterRef

case class InterpreterFunctionRef(params: Seq[Seq[Term.Param]], tparams: Seq[Type.Param], body: Tree, capturedEnv: Env)
    extends InterpreterRef {
}

case class InterpreterJvmRef(tpe: Type) extends InterpreterRef

object InterpreterRef {
  def wrap[T](value: T, env: Env, tpe: Type): (InterpreterRef, Env) = {
    val ref = InterpreterJvmRef(tpe)
    (ref, env.extend(ref, InterpreterPrimitive(value)))
  }
}
