package org.scalameta.interpreter.internal.environment

import scala.meta._

case class ClassInfo(constructor: InterpreterCtorRef)

case class ClassTable(table: Map[Symbol, ClassInfo])
