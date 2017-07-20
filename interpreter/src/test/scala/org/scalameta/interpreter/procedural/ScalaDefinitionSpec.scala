package org.scalameta.interpreter.procedural

import org.scalameta.interpreter._

import scala.meta._

class ScalaDefinitionSpec extends ScalametaInterpreterSpec with ScalametaInterpreterDefault {
  it should "handle simple val initialization" in {
    checkCode(q"val x = 7", (), Seq(("x", 7)))
  }

  it should "handle simple var initialization" in {
    checkCode(q"var x = 7", (), Seq(("x", 7)))
  }

  it should "handle val-val initialization" in {
    checkCode(q"val x = 7; val y = x; y", 7, Seq(("x", 7), ("y", 7)))
  }

  it should "handle val-var initialization" in {
    checkCode(q"val x = 7; var y = x; y", 7, Seq(("x", 7), ("y", 7)))
  }

  it should "handle var-val initialization" in {
    checkCode(q"var x = 7; val y = x; y", 7, Seq(("x", 7), ("y", 7)))
  }

  it should "handle var-var initialization" in {
    checkCode(q"var x = 7; var y = x; y", 7, Seq(("x", 7), ("y", 7)))
  }

  it should "handle mutable var" in {
    checkCode(q"var x = 1; x = 7; x", 7, Seq(("x", 7)))
  }

  it should "handle transitive immutability" in {
    checkCode(q"val x = 1; var y = x; y = 7; y", 7, Seq(("x", 1), ("y", 7)))
  }

  it should "handle var initialization without body" in {
    checkCode(
      q"var x: Byte = _",
      (),
      Seq(("x", 0.toByte))
    )
    checkCode(
      q"var x: Short = _",
      (),
      Seq(("x", 0.toShort))
    )
    checkCode(
      q"var x: Char = _",
      (),
      Seq(("x", 0.toChar))
    )
    checkCode(q"var x: Int = _",
      (),
      Seq(("x", 0))
    )
    checkCode(q"var x: Long = _",
      (),
      Seq(("x", 0l))
    )
    checkCode(
      q"var x: Float = _",
      (),
      Seq(("x", 0.0f))
    )
    checkCode(
      q"var x: Double = _",
      (),
      Seq(("x", 0.0d))
    )
    checkCode(
      q"var x: Boolean = _",
      (),
      Seq(("x", false))
    )
    checkCode(q"var x: Unit = _",
      (),
      Seq(("x", ()))
    )
    checkCode(
      q"var x: String = _",
      (),
      Seq(("x", null))
    )
  }
}
