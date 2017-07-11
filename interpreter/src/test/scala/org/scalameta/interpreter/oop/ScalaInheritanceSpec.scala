package org.scalameta.interpreter.oop

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaInheritanceSpec extends ScalametaInterpreterSpec {
  it should "be able to instantiate classes with parents" in {
    checkCode(
      q"""
         class A(val a1: Int, val a2: Double) {
           val ax = a1 * 3
         }

         class B(val b1: Int, val b2: Double) extends A(b1, b2)

         val b = new B(2, 3.0)
         b.ax + b.b1
       """, 8, Seq())
  }

  it should "be able to call parent methods" in {
    checkCode(
      q"""
         class A(val a1: Int, val a2: Double) {
           val ax = a1 * 3

           def fooAI(x: Int): Int = ax / x
         }

         class B(val b1: Int, val b2: Double) extends A(b1, b2)

         val b = new B(2, 3.0)
         b.fooAI(2)
       """, 3, Seq())
  }
}