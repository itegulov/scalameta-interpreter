package org.scalameta.interpreter.internal

import org.scalameta.interpreter.ScalametaMirror

import scala.meta._

package object environment {
  type Heap       = Map[InterpreterRef, InterpreterValue]
  type FrameStack = List[Map[Symbol, InterpreterRef]]
}
