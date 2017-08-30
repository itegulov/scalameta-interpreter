package org.scalameta.interpreter

import org.scalameta.interpreter.internal.Engine

import scala.meta._

object Main extends App {
  implicit val mirror = ScalametaMirrorImpl
  val sourceFile = scala.io.Source.fromFile(args(0))
  val sourceContent = sourceFile.getLines().mkString("\n")
  sourceFile.close
  sourceContent.parse[Source] match {
    case Parsed.Success(source) =>
      val (ref, env) = Engine.eval(source)
      val value = ref.reify(env)
      println(s"Interpreting resulted in $value")
    case e: Parsed.Error =>
      println("Invalid Scala source file:")
      println(e)
  }
}
