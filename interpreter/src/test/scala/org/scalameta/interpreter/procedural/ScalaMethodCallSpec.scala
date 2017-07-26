package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.{ScalametaInterpreterDefault, ScalametaInterpreterSpec}

import scala.meta._

class ScalaMethodCallSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault {
  it should "be able to call simple Java methods" in {
    checkCode(q"7.equals(7)", true, Seq())
    checkCode(q"7.equals(8)", false, Seq())
    checkCode(q"7.hashCode()", 7, Seq())
  }

  it should "be able to call infix arithmetic methods" in {
    checkCode(q"2 + 2", 4, Seq())
    checkCode(q"2 - 1", 1, Seq())
    checkCode(q"2 * 3", 6, Seq())
    checkCode(q"27 / 3", 9, Seq())
  }
  
  it should "be able to call infix boolean methods" in {
    checkCode(q"true && false", false, Seq())
    checkCode(q"true || false", true, Seq())
  }

  it should "be able to call infix equals" in {
    checkCode(q"2 == 2", true, Seq())
    checkCode(q"2 != 2", false, Seq())
    checkCode(q"1.0 == 2", false, Seq())
  }


  it should "be able to call unary operators" in {
    checkCode(q"!false", true, Seq())
  }
}
