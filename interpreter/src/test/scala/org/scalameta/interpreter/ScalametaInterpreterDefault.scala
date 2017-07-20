package org.scalameta.interpreter

trait ScalametaInterpreterDefault {
  implicit val mirror = ScalametaMirrorImpl
}
