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

  it should "handle simple val definitions" in {
    checkCode(q"val x = 7", InterpreterPrimitive(()), Seq(("x", InterpreterPrimitive(7))))
  }

  it should "handle simple var definitions" in {
    checkCode(q"var x = 7", InterpreterPrimitive(()), Seq(("x", InterpreterPrimitive(7))))
  }

  it should "handle var definitions without body" in {
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
