package org.scalameta.interpreter.oop

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaInheritanceSpec extends ScalametaInterpreterSpec {
  it should "be able to instantiate classes with parents" in {
    checkCode(
      q"""
         class A(val x: Int) {
           val z = x * 3
         }

         class B(val x: Int, val y: Double) extends A(x)

         val b = new B(2, 3.0)
         b.z + b.x
       """, 8, Seq())
  }

  it should "be able to call parent methods" in {
    checkCode(
      q"""
         class A(val x: Int) {
           val z = x * 3

           def foo(f: Int): Int = z / f
         }

         class B(val x: Int, val y: Double) extends A(x)

         val b = new B(2, 3.0)
         b.foo(2)
       """, 3, Seq())
  }

  it should "handle child overridden fields" in {
    checkCode(
      q"""
         class A {
           val x: Int

           def foo(f: Int): Int = x / f
         }

         class B(val x: Int) extends A

         val b = new B(8)
         b.foo(2)
       """, 4, Seq())
  }
}
