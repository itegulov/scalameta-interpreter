package org.scalameta.interpreter.internal.environment

import scala.meta._

sealed trait InterpreterValue

final case class InterpreterPrimitive(value: Any) extends InterpreterValue

final case class InterpreterWrappedJvm(value: Any) extends InterpreterValue

final case class InterpreterObject(classSymbol: Symbol, fields: Map[Symbol, InterpreterRef])
    extends InterpreterValue {
  def extend(fieldName: Symbol, fieldRef: InterpreterRef): InterpreterObject =
    InterpreterObject(classSymbol, fields + (fieldName -> fieldRef))
}
