package org.scalameta.interpreter.oop

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaObjectSpec extends ScalametaInterpreterSpec {
  it should "be able to create simple objects" in {
    checkCode(
      q"""
         object A {
         }
       """, (), Seq())
  }

  it should "be able to access object fields" in {
    checkCode(
      q"""
         object A {
           val x = 7
         }
         A.x
       """, 7, Seq())
  }

  it should "be able to access object functions" in {
    checkCode(
      q"""
         object A {
           def foo(x: Int) = x * 7
         }
         A.foo(7)
       """, 49, Seq())
  }
}
