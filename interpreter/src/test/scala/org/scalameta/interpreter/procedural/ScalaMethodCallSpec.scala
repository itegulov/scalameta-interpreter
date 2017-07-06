package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaMethodCallSpec extends ScalametaInterpreterSpec {
  it should "be able to call simple methods" in {
    checkCode(q"7.equals(7)", true, Seq())
    checkCode(q"7.equals(8)", false, Seq())
  }
}
