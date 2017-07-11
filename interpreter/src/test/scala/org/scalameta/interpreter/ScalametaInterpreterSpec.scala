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
  def checkCode(code: Tree,
                expectedResult: InterpreterValue,
                expectedSymbols: Seq[(String, InterpreterValue)]): (InterpreterRef, Env) = {
    implicit val mirror = ScalametaMirrorImpl
    val (ref, env) = Engine.eval(code)
    env.heap.get(ref).value should be(expectedResult)
    for ((symbolName, symbolValue) <- expectedSymbols) {
      val symbolRef = env.stack.headOption.value.get(Symbol.Local(symbolName)).value
      env.heap.get(symbolRef).value should be(symbolValue)
    }
    (ref, env)
  }

  implicit def intToPrimitive(int: Int): InterpreterPrimitive = InterpreterPrimitive(int)

  implicit def doubleToPrimitive(double: Double): InterpreterPrimitive = InterpreterPrimitive(double)

  implicit def unitToPrimitive(unit: Unit): InterpreterPrimitive = InterpreterPrimitive(unit)

  implicit def booleanToPrimitive(bool: Boolean): InterpreterPrimitive = InterpreterPrimitive(bool)

  implicit def stringToPrimitive(str: String): InterpreterPrimitive = InterpreterPrimitive(str)
}
