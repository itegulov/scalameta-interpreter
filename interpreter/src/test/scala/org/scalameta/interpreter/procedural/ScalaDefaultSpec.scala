package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.{ScalametaInterpreterDefault, ScalametaInterpreterSpec}

import scala.meta._

class ScalaDefaultSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault  {
  it should "support default method parameters" in {
    checkCode(
      q"""
         def fooI(x: Int = 7): Int = x * 2
          
         fooI(1) + fooI()
       """, 16, Seq())
  }
}
