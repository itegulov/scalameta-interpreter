package org.scalameta.interpreter.oop

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaClassSpec extends ScalametaInterpreterSpec {
  it should "handle simple classes" in {
    checkCode(
      q"""
         class A(val a: Int, val b: Double)
       """, (), Seq())
  }

  it should "be able to construct defined class" in {
    checkCode(
      q"""
         class A(val x: Int, val y: Double)
         val a = new A(1, 1.0)
       """, (), Seq())
  }

  it should "be able to access constructed classes field" in {
    checkCode(
      q"""
         class A(val x: Int, val y: Double)
         val a = new A(1, 1.0)
         a.x
       """, 1, Seq())

    checkCode(
      q"""
         class A(val x: Int, val y: Double)
         val a = new A(1, 1.0d)
         a.y
       """, 1.0, Seq())
  }

  it should "be able to access constructed classes methods" in {
    checkCode(
      q"""
         class A(val x: Int, val y: Double) {
           def foo(z: String): String = z
         }
         val a = new A(1, 1.0)
         a.foo("1")
       """, "1", Seq())
  }
}
