package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaMethodCallSpec extends ScalametaInterpreterSpec {
  it should "be able to call simple methods" in {
    checkCode(q"7.equals(7)", true, Seq())
    checkCode(q"7.equals(8)", false, Seq())
  }

  it should "be able to call infix methods" in {
    checkCode(q"2 + 2", 4, Seq())
    checkCode(q"2 - 1", 1, Seq())
    checkCode(q"2 * 3", 6, Seq())
    checkCode(q"27 / 3", 9, Seq())
  }
}
