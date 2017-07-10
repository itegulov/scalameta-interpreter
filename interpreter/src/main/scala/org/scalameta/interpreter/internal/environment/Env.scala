package org.scalameta.interpreter.internal.environment

import scala.collection.immutable.ListMap

case class Env(stack: FrameStack, heap: Heap, classTable: ClassTable, thisContext: ListMap[ClassName, InterpreterRef]) {
  def extendHeap(otherHeap: Heap): Env = Env(stack, heap ++ otherHeap, classTable, thisContext)

  def pushFrame(frame: Map[String, InterpreterRef]): Env =
    Env(frame +: stack, heap, classTable, thisContext)

  def extend(ref: InterpreterRef, value: InterpreterValue): Env =
    Env(stack, heap + (ref -> value), classTable, thisContext)

  def extend(sym: String, ref: InterpreterRef): Env =
    stack.head.get(sym) match {
      case Some(_) => Env((stack.head + (sym -> ref)) :: stack.tail, heap, classTable, thisContext)
      case None    => Env((stack.head + (sym -> ref)) :: stack.tail, heap, classTable, thisContext)
    }

  def addClass(className: ClassName, classInfo: ClassInfo): Env =
    Env(stack, heap, ClassTable(classTable.table + (className -> classInfo)), thisContext)

  def addThis(className: ClassName, interpreterRef: InterpreterRef): Env =
    Env(stack, heap, classTable, thisContext + (className -> interpreterRef))
}
