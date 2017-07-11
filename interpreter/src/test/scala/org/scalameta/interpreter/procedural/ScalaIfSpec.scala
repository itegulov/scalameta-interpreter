package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.ScalametaInterpreterSpec
import scala.meta._

class ScalaIfSpec extends ScalametaInterpreterSpec {
  it should "handle basic true if expression" in {
    checkCode(q"if (true) 1 else 2", 1, Seq())
  }

  it should "handle basic false if expression" in {
    checkCode(q"if (false) 1 else 2", 2, Seq())
  }

  it should "handle comparison if expression" in {
    checkCode(
      q"""
          val x = 1
          if (x == 1) 1 else 2
        """, 1, Seq())
    checkCode(
      q"""
          val x = 1
          if (x != 1) 1 else 2
        """, 2, Seq())
  }

  it should "handle nested if expressions" in {
    checkCode(
      q"""
          val x = 1
          var y = 1
          if (x == 1) {
            y = 2
            if (x != 2) {
              y = 3
            } else {
              y = 4
            }
          } else {
            y = 5
          }
          y
        """, 3, Seq())
    checkCode(
      q"""
          val x = 1
          var y = 1
          if (x == 1) {
            y = 2
            if (x != 1) {
              y = 3
            } else {
              y = 4
            }
          } else {
            y = 5
          }
          y
        """, 4, Seq())
  }

  it should "handle if expression with mutable variables" in {
    checkCode(
      q"""
          var x = 1
          var y = 1
          if (x == 1) {
            x = 2
            y = 2
          }
          if (x == 2) {
            x = 3
            y = 3
          }
          y
        """, 3, Seq())
  }
}
