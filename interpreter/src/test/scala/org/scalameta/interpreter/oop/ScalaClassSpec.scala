package org.scalameta.interpreter.oop

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaClassSpec extends ScalametaInterpreterSpec {
  it should "handle simple classes" in {
    checkCode(q"""
         class A(val a: Int, val b: Double)
       """, (), Seq())
  }

  it should "be able to construct defined class" in {
    checkCode(q"""
         class A(val x: Int, val y: Double)
         val a = new A(1, 1.0)
       """, (), Seq())
  }

  it should "be able to access constructed classes field" in {
    checkCode(q"""
         class A(val x: Int, val y: Double)
         val a = new A(1, 1.0)
         a.x
       """, 1, Seq())

    checkCode(q"""
         class A(val x: Int, val y: Double)
         val a = new A(1, 1.0d)
         a.y
       """, 1.0, Seq())
  }

  it should "be able to access constructed classes methods" in {
    checkCode(q"""
         class A(val x: Int, val y: Double) {
           def foo(z: String): String = z
         }
         val a = new A(1, 1.0)
         a.foo("1")
       """, "1", Seq())
  }

  it should "be able to access fields in methods" in {
    checkCode(q"""
         class A(val x: Int, val y: Double) {
           def foo(z: Int): Int = z * x
         }
         val a = new A(2, 1.0)
         a.foo(3)
       """, 6, Seq())
  }

  it should "handle computed fields properly" in {
    checkCode(q"""
         class A(val x: Int, val y: Double) {
           val z: Int = x * 2
         }
         val a = new A(1, 1.0)
         a.z
       """, 2, Seq())
  }

  it should "handle fields in scopes properly" in {
    checkCode(q"""
         val z = 3
         class A(val x: Int, val y: Double) {
           val z: Int = x * 2
         }
         val a = new A(1, 1.0)
         a.z + z
       """, 5, Seq())
  }

  it should "handle functions in scopes properly" in {
    checkCode(q"""
         def foo(z: Int): Int = z * 3
         class A(val x: Int, val y: Double) {
           def foo(z: Int): Int = z * x
         }
         val a = new A(2, 1.0)
         a.foo(3) + foo(3)
       """, 15, Seq())
  }

  it should "handle mutable classes properly" in {
    checkCode(q"""
      class A(var x: Int) {
        def foo(y: Int) = x * y
      }
      val a = new A(2)
      a.foo(3)
      a.x = 7
      a.foo(3)
    """, 21, Seq())
  }
}
