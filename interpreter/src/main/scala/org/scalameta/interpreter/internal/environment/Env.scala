package org.scalameta.interpreter.internal.environment

import scala.collection.immutable.ListMap

import scala.meta._

case class Env(stack: FrameStack, heap: Heap, classTable: ClassTable, thisContext: ListMap[Symbol, InterpreterRef]) {
  def extendHeap(otherHeap: Heap): Env = Env(stack, heap ++ otherHeap, classTable, thisContext)

  def pushFrame(frame: Map[Symbol, InterpreterRef]): Env =
    Env(frame +: stack, heap, classTable, thisContext)

  def extend(ref: InterpreterRef, value: InterpreterValue): Env =
    Env(stack, heap + (ref -> value), classTable, thisContext)

  def extend(sym: Symbol, ref: InterpreterRef): Env =
    stack.head.get(sym) match {
      case Some(_) => Env((stack.head + (sym -> ref)) :: stack.tail, heap, classTable, thisContext)
      case None    => Env((stack.head + (sym -> ref)) :: stack.tail, heap, classTable, thisContext)
    }

  def addClass(classSymbol: Symbol, classInfo: ClassInfo): Env =
    Env(stack, heap, ClassTable(classTable.table + (classSymbol -> classInfo)), thisContext)

  def addThis(classSymbol: Symbol, interpreterRef: InterpreterRef): Env =
    Env(stack, heap, classTable, thisContext + (classSymbol -> interpreterRef))
}
