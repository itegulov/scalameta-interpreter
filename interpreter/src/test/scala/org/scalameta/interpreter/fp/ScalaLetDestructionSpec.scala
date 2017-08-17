package org.scalameta.interpreter.fp

import org.scalameta.interpreter.{ScalametaInterpreterDefault, ScalametaInterpreterSpec}

import scala.meta._

class ScalaLetDestructionSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault {
  it should "be able to destruct pairs" in {
    checkCode(
      q"""
         val (x, y) = (1, 2)
         x + y
       """, 3, Seq())
  }

  it should "be able to destruct Seq" in {
    checkCode( 
      q"""
         val Seq(x, y) = Seq(1, 2)
         x + y
       """, 3, Seq())
  }

  ignore should "be able to destruct List through ::" in {
    checkCode(
      q"""
         val x :: y = List(1, 2)
         x + y
       """, 3, Seq())
  }

  it should "be able to destruct Options" in {
    checkCode(
      q"""
         val Some(x) = Some(1)
         x
       """, 1, Seq())
  }
}
