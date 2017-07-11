package org.scalameta.interpreter.oop

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaClassSpec extends ScalametaInterpreterSpec {
  it should "handle simple classes" in {
    checkCode(q"""
         class A(val a1: Int, val a2: Double)
       """, (), Seq())
  }

  it should "be able to construct defined class" in {
    checkCode(q"""
         class A(val a1: Int, val a2: Double)
         val a = new A(1, 1.0)
       """, (), Seq())
  }

  it should "be able to access constructed classes field" in {
    checkCode(q"""
         class A(val a1: Int, val a2: Double)
         val a = new A(1, 1.0)
         a.a1
       """, 1, Seq())

    checkCode(q"""
         class A(val a1: Int, val a2: Double)
         val a = new A(1, 1.0d)
         a.a1
       """, 1.0, Seq())
  }

  it should "be able to access constructed classes methods" in {
    checkCode(q"""
         class A(val a1: Int, val a2: Double) {
           def fooAS(x: String): String = x
         }
         val a = new A(1, 1.0)
         a.fooAS("1")
       """, "1", Seq())
  }

  it should "be able to access fields in methods" in {
    checkCode(q"""
         class A(val a1: Int, val a2: Double) {
           def fooAI(x: Int): Int = x * a1
         }
         val a = new A(2, 1.0)
         a.fooAI(3)
       """, 6, Seq())
  }

  it should "handle computed fields properly" in {
    checkCode(q"""
         class A(val a1: Int, val a2: Double) {
           val ax: Int = a1 * 2
         }
         val a = new A(1, 1.0)
         a.ax
       """, 2, Seq())
  }

  it should "handle fields in scopes properly" in {
    checkCode(q"""
         val x = 3
         class A(val a1: Int, val a2: Double) {
           val ax: Int = a1 * 2
         }
         val a = new A(1, 1.0)
         a.ax + x
       """, 5, Seq())
  }

  it should "handle functions in scopes properly" in {
    checkCode(q"""
         def fooI(x: Int): Int = x * 3
         class A(val a1: Int, val a2: Double) {
           def fooAI(x: Int): Int = a1 * x
         }
         val a = new A(2, 1.0)
         a.fooAI(3) + fooI(3)
       """, 15, Seq())
  }

  it should "handle mutable classes properly" in {
    checkCode(q"""
      class A(var a1: Int, val a2: Double) {
        def fooI(x: Int) = x * a1
      }
      val a = new A(2, 1.0)
      a.fooI(3)
      a.a1 = 7
      a.fooI(3)
    """, 21, Seq())
  }
}
