package org.scalameta.interpreter.fp

import org.scalameta.interpreter.ScalametaInterpreterSpec

import scala.meta._

class ScalaLambdaSpec extends ScalametaInterpreterSpec {
  it should "interpret simple lambdas" in {
    checkCode(
      q"""
         val x = (y: Int) => y
         x(7)
       """, 7, Seq())
  }
}
