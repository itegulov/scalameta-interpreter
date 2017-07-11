package org.scalameta.interpreter.oop

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaObjectSpec extends ScalametaInterpreterSpec {
  it should "be able to create simple objects" in {
    checkCode(
      q"""
         object OA {
         }
       """, (), Seq())
  }

  it should "be able to access object fields" in {
    checkCode(
      q"""
         object OA {
           val oax = 7
         }
         OA.oax
       """, 7, Seq())
  }

  it should "be able to access object functions" in {
    checkCode(
      q"""
         object OA {
           def fooOAI(x: Int) = x * 7
         }
         OA.fooOAI(7)
       """, 49, Seq())
  }
}
