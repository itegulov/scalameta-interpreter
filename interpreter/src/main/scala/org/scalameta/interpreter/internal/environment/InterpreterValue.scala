package org.scalameta.interpreter.internal.environment

sealed trait InterpreterValue

final case class InterpreterPrimitive(value: Any) extends InterpreterValue

final case class InterpreterObject(className: ClassName, fields: Map[String, InterpreterRef]) extends InterpreterValue {
  def extend(fieldName: String, fieldRef: InterpreterRef): InterpreterObject =
    InterpreterObject(className, fields + (fieldName -> fieldRef))
}
