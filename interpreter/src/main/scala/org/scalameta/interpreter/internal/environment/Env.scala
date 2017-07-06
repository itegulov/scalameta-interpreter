package org.scalameta.interpreter.internal.environment

case class Env(stack: FrameStack, heap: Heap) {
  def extendHeap(otherHeap: Heap): Env = Env(stack, heap ++ otherHeap)

  def pushFrame(frame: Map[String, InterpreterRef]): Env =
    Env(frame +: stack, heap)

  def extend(ref: InterpreterRef, primitive: InterpreterPrimitive): Env =
    Env(stack, heap + (ref -> primitive))

  def extend(sym: String, ref: InterpreterRef): Env =
    stack.head.get(sym) match {
      case Some(_) => Env((stack.head + (sym -> ref)) :: stack.tail, heap)
      case None    => Env((stack.head + (sym -> ref)) :: stack.tail, heap)
    }
}
