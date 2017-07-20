package org.scalameta.interpreter

import com.typesafe.scalalogging.StrictLogging
import org.scalameta.interpreter.internal.Engine
import org.scalameta.interpreter.internal.environment._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.meta._

class ScalametaInterpreterSpec
    extends FlatSpec
    with Matchers
    with Inspectors
    with Inside
    with OptionValues
    with EitherValues
    with TryValues
    with ScalaFutures
    with StrictLogging {
  def checkCode(
    code: Tree,
    expectedResult: Any,
    expectedSymbols: Seq[(String, Any)]
  )(implicit mirror: ScalametaMirror): (InterpreterRef, Env) = {
    val (ref, env) = Engine.eval(code)
    ref.reify(env) should be(expectedResult)
    for ((symbolName, symbolValue) <- expectedSymbols) {
      val symbolRef = env.stack.headOption.value.get(Symbol.Local(symbolName)).value
      symbolRef.reify(env) should be(symbolValue)
    }
    (ref, env)
  }
}
