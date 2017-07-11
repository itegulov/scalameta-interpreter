package org.scalameta.interpreter.predef

import org.scalameta.interpreter.ScalametaInterpreterSpec
import org.scalameta.interpreter.internal.environment.InterpreterWrappedJvm

import scala.meta._

class ScalaPredefSpec extends ScalametaInterpreterSpec {
  it should "be able to call predef classes" in {
    checkCode(
      q"""
         println("test")
       """, InterpreterWrappedJvm(()), Seq())
  }
}
