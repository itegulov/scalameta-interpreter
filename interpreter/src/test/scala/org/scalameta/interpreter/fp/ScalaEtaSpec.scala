package org.scalameta.interpreter.fp

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaEtaSpec extends ScalametaInterpreterSpec {
  it should "handle simple eta expansion" in {
    checkCode(
      q"""
          def fooI(x: Int) = x * 2
          val x = fooI _
          x(3)
       """, 6, Seq())
  }

  it should "handle value eta expansion" in {
    checkCode(
      q"""
          val x: String = "test"
          val y = x _
          y()
       """, "test", Seq())
  }
}
