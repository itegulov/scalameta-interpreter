package org.scalameta.interpreter.other

import org.scalameta.interpreter.{ScalametaInterpreterSpec, ScalametaMirror}

import scala.meta._

class ScalaUpdateSpec extends ScalametaInterpreterSpec {
  implicit val mirror: ScalametaMirror = {
    case Term.Name("update") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Term("collection")), Signature.Term("mutable")), Signature.Type("MapLike")), Signature.Method("update", "(Ljava/lang/Object;Ljava/lang/Object;)V"))
  }
  
  it should "be able to update fields" in {
    checkCode(
      q"""
         val x = Map()
         x(1) = 2
         x(1)
       """, 2, Seq())
  }
}
