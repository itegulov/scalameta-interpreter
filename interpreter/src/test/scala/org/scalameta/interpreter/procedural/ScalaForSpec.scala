package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.{ScalametaInterpreterDefault, ScalametaInterpreterSpec}

import scala.meta._

class ScalaForSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault {

  it should "process simple for loops" in {
    checkCode(
      q"""
         var y = 0
         for (x <- Seq(1, 2, 3)) {
           y = y + x
         }
         y
       """, 6, Seq())
  }

  it should "process for loops with guards" in {
    checkCode(
      q"""
         var y = 0
         for {
           x <- Seq(1, 2, 3)
           if x != 2
         } {
           y = y + x
         }
         y
       """, 4, Seq())
  }

  it should "process for loops with let expressions" in {
    checkCode(
      q"""
         var y = 0
         for {
           x <- Seq(1, 2, 3)
           z = x + 1
         } {
           y = y + z
         }
         y
       """, 9, Seq())
  }

  it should "process for loops with flatMaps" in {
    checkCode(
      q"""
         var z = 0
         for {
           x <- Seq(1, 2, 3)
           y <- Seq(1, 2, 3)
         } {
           z = z + x * y
         }
         z
       """, 36, Seq())
  }
}
