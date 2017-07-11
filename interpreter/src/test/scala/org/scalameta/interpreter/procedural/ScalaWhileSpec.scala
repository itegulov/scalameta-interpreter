package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaWhileSpec extends ScalametaInterpreterSpec {
  it should "handle zero iterations" in {
    checkCode(
      q"""
         var x = 100
         while (x < 100) {
           x = x + 1
         }
         x
       """, 100, Seq())
  }

  it should "handle hundred iterations with mutable variable" in {
    checkCode(
      q"""
         var x = 0
         while (x < 100) {
           x = x + 1
         }
         x
       """, 100, Seq())
  }

  it should "calculate sum of 1..100" in {
    checkCode(
      q"""
         var x = 0
         var y = 0
         while (x < 100) {
           x = x + 1
           y = y + x
         }
         y
       """, 5050, Seq())
  }

  it should "calculate sum of 1..100 through nested whiles" in {
    checkCode(
      q"""
         var x = 0
         var y = 0
         var z = 0
         while (x < 10) {
           var y = 0
           while (y < 10) {
             y = y + 1
             z = z + x * 10 + y
           }
           x = x + 1
         }
         z
       """, 5050, Seq())
  }

  it should "handle mutable effects in conditions" in {
    checkCode(
      q"""
         var x = 100
         var y = 0
         while ({
           y = 100
           x < 100
         }) {
           y = 10
         }
         y
       """, 100, Seq())
  }

  it should "handle do while" in {
    checkCode(
      q"""
         var x = 0
         do {
           x = x + 1
         } while (x < 100)
         x
       """, 100, Seq())
  }
}
