package org.scalameta.interpreter.internal.environment

case class Env(stack: FrameStack, heap: Heap) {
  def extend(ref: InterpreterRef, primitive: InterpreterPrimitive): Env =
    Env(stack, heap + (ref -> primitive))
  def extend(sym: String, ref: InterpreterRef): Env = {
    stack.head.get(sym) match {
      case Some(_) => Env((stack.head + (sym -> ref)) :: stack.tail, heap)
      case None => Env((stack.head + (sym -> ref)) :: stack.tail, heap)
    }
  }
}
