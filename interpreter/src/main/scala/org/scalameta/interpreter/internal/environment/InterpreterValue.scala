package org.scalameta.interpreter.internal.environment

sealed trait InterpreterValue

final case class InterpreterPrimitive(value: Any) extends InterpreterValue

final case class InterpreterObject(value: Any) extends InterpreterValue
