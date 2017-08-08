package org.scalameta.interpreter.other

import org.scalameta.interpreter.{ScalametaInterpreterDefault, ScalametaInterpreterSpec}

import scala.meta._

class ScalaImplicitSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault {
  it should "convert String to StringOps for explicit StringOps method invocations" in {
    checkCode(q""" "implicit".toList """, List('i', 'm', 'p', 'l', 'i', 'c', 'i', 't'), Seq())
  }
  
  it should "convert String to StringOps for operators" in {
    checkCode(q""" "implicit" < "b" """, false, Seq())
  }
}
