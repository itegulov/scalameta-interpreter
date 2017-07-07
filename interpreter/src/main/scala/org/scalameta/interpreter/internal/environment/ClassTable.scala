package org.scalameta.interpreter.internal.environment

case class ClassName(name: String) extends AnyVal

case class ClassInfo(constructor: InterpreterCtorRef)

case class ClassTable(table: Map[ClassName, ClassInfo])
