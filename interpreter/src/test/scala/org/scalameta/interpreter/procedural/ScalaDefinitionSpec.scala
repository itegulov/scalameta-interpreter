package org.scalameta.interpreter.procedural

import org.scalameta.interpreter.ScalametaInterpreterSpec
import org.scalameta.interpreter.internal.Engine
import org.scalameta.interpreter.internal.environment._

import scala.meta._

class ScalaDefinitionSpec extends ScalametaInterpreterSpec {
  def checkCode(code: Tree,
                expectedResult: InterpreterValue,
                expectedSymbols: Seq[(String, InterpreterValue)]): (InterpreterRef, Env) = {
    val (ref, env) = Engine.eval(code)
    env.heap.get(ref).value should be(expectedResult)
    for ((symbolName, symbolValue) <- expectedSymbols) {
      val symbolRef = env.stack.headOption.value.get(symbolName).value
      env.heap.get(symbolRef).value should be(symbolValue)
    }
    (ref, env)
  }

  implicit def intToPrimitive(int: Int): InterpreterPrimitive = InterpreterPrimitive(int)

  it should "handle simple val initialization" in {
    checkCode(q"val x = 7", InterpreterPrimitive(()), Seq(("x", 7)))
  }

  it should "handle simple var initialization" in {
    checkCode(q"var x = 7", InterpreterPrimitive(()), Seq(("x", 7)))
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
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(0.toByte)))
    )
    checkCode(
      q"var x: Short = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(0.toShort)))
    )
    checkCode(
      q"var x: Char = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(0.toChar)))
    )
    checkCode(q"var x: Int = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(0)))
    )
    checkCode(q"var x: Long = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(0l)))
    )
    checkCode(
      q"var x: Float = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(0.0f)))
    )
    checkCode(
      q"var x: Double = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(0.0d)))
    )
    checkCode(
      q"var x: Boolean = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(false)))
    )
    checkCode(q"var x: Unit = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(())))
    )
    checkCode(
      q"var x: String = _",
      InterpreterPrimitive(()),
      Seq(("x", InterpreterPrimitive(null)))
    )
  }
}
