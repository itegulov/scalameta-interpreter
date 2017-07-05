package org.scalameta.interpreter.internal

package object environment {
  type Heap = Map[InterpreterRef, InterpreterValue]
  type FrameStack = List[Map[String, InterpreterRef]]
}
