package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.{ScalametaInterpreterDefault, ScalametaInterpreterSpec}

import scala.meta._

class ScalaReturnSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault {
  it should "be able to return values explicitly" in {
    checkCode(
      q"""
         def fooI(x: Int) = {
           if (x <= 0) return -1
           x
         }
         fooI(0)
       """, -1, Seq())
  }

  it should "be able to return values explicitly in class methods" in {
    checkCode(
      q"""
         class A(val a1: Int, val a2: Double) {
           def fooAI(x: Int) = {
             if (x <= a1) return -1
             x
           }
         }
         val a = new A(1, 2.0)
         a.fooAI(1)
       """, -1, Seq())
  }

  it should "handle nested returns" in {
    checkCode(
      q"""
         def barI(x: Int): Int = {
           return x + 2
           999
         }
         def fooI(x: Int): Int = {
           return barI(x + 1)
           888
         }
         fooI(7)
       """, 10, Seq())
  }

  it should "handle return from while" in {
    checkCode(
      q"""
         def fooI(x: Int): Int = {
           var y = 0
           var z = 0
           while (true) {
             if (y == x) return z
             y = y + 1
             z = z + y
           }
         }
         fooI(100)
       """, 5050, Seq())
  }
}
