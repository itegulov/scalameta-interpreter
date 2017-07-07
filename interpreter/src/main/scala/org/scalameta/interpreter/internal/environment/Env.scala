package org.scalameta.interpreter.internal.environment

case class Env(stack: FrameStack, heap: Heap, classTable: ClassTable) {
  def extendHeap(otherHeap: Heap): Env = Env(stack, heap ++ otherHeap, classTable)

  def pushFrame(frame: Map[String, InterpreterRef]): Env =
    Env(frame +: stack, heap, classTable)

  def extend(ref: InterpreterRef, primitive: InterpreterPrimitive): Env =
    Env(stack, heap + (ref -> primitive), classTable)

  def extend(sym: String, ref: InterpreterRef): Env =
    stack.head.get(sym) match {
      case Some(_) => Env((stack.head + (sym -> ref)) :: stack.tail, heap, classTable)
      case None    => Env((stack.head + (sym -> ref)) :: stack.tail, heap, classTable)
    }

  def addClass(className: ClassName, classInfo: ClassInfo): Env =
    Env(stack, heap, ClassTable(classTable.table + (className -> classInfo)))
}
