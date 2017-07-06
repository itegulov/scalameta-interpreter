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
}
