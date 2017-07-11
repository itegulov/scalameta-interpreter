package org.scalameta.interpreter.internal.environment

import scala.language.dynamics

import scala.meta._

case class InterpreterDynamic(fields: Map[Symbol, InterpreterRef]) extends Dynamic {
  def selectDynamic(name: Symbol): InterpreterRef = fields(name)
}
