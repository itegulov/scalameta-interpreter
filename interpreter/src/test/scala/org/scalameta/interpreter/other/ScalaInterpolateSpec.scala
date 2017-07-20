package org.scalameta.interpreter.other

import org.scalameta.interpreter.{ScalametaInterpreterDefault, ScalametaInterpreterSpec}

import scala.meta._

class ScalaInterpolateSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault {
  it should "interpolate simple strings" in {
    checkCode(
      q"""
         val x = 1
         s"foo $$x bar"
       """, "foo 1 bar", Seq())
    checkCode(
      q"""
         val x = 1
         s"$$x bar"
       """, "1 bar", Seq())
    checkCode(
      q"""
         val x = 1
         s"foo $$x"
       """, "foo 1", Seq())
  }

  it should "interpolate expressions in strings" in {
    checkCode(
      q"""
         val x = 1
         val y = 2
         s"foo $${x + y} bar"
       """, "foo 3 bar", Seq())
    checkCode(
      q"""
         val x = 1
         val y = 2
         s"$${x + y} bar"
       """, "3 bar", Seq())
    checkCode(
      q"""
         val x = 1
         val y = 2
         s"foo $${x + y}"
       """, "foo 3", Seq())
  }

  it should "interpolate multiple terms in strings" in {
    checkCode(
      q"""
         val x = 1
         val y = 2
         s"foo $$x bar $$y baz"
       """, "foo 1 bar 2 baz", Seq())
    checkCode(
      q"""
         val x = 1
         val y = 2
         s"$$x bar $$y"
       """, "1 bar 2", Seq())
    checkCode(
      q"""
         val x = 1
         val y = 2
         s"foo $$x $$y baz"
       """, "foo 1 2 baz", Seq())
  }
}
