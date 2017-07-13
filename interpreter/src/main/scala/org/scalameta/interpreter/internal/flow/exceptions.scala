package org.scalameta.interpreter.internal.flow

import org.scalameta.interpreter.internal.environment.{Env, InterpreterRef}

object exceptions {
  final case class ReturnException(interpreterRef: InterpreterRef, env: Env) extends Exception
  final case class InterpreterException(interpreterRef: InterpreterRef, env: Env) extends Exception
}
