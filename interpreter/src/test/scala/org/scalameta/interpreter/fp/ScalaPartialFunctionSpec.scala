package org.scalameta.interpreter.fp

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaPartialFunctionSpec extends ScalametaInterpreterSpec {
  it should "interpret simple lambdas" in {
    checkCode(
      q"""
         val x = {
           case 1 => 10
           case 2 => 20
         }
         x(1)
       """, 10, Seq())
    checkCode(
      q"""
         val x = {
           case 1 => 10
           case 2 => 20
         }
         x(2)
       """, 20, Seq())
  }
}
