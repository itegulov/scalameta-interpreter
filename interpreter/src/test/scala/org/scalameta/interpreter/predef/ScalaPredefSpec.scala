package org.scalameta.interpreter.predef

import org.scalameta.interpreter.{ScalametaInterpreterDefault, ScalametaInterpreterSpec}

import scala.meta._

class ScalaPredefSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault {
  it should "be able to call predef functions" in {
    checkCode(
      q"""
         println("test")
       """, (), Seq())
  }

  it should "be able to construct predef classes" in {
    checkCode(
      q"""
         val x = Seq(1, 2, 3)
       """, (), Seq())
  }

  it should "invoke predef class methods" in {
    checkCode(
      q"""
         val x = Seq(1, 2, 3)
         x(0)
       """, 1, Seq())
  }
}
