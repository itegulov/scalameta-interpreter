package org.scalameta.interpreter.fp

import org.scalameta.interpreter.ScalametaInterpreterSpec
import org.scalameta.interpreter.internal.environment.InterpreterWrappedJvm

import scala.meta._

class ScalaMatchSpec extends ScalametaInterpreterSpec {
  it should "match integers" in {
    checkCode(
      q"""
         3 match {
           case 1 => 11
           case 2 => 12
           case 3 => 13
           case _ => -1
         }
       """, 13, Seq())
    checkCode(
      q"""
         8 match {
           case 1 => 11
           case 2 => 12
           case 3 => 13
           case _ => -1
         }
       """, -1, Seq())
  }

  it should "match integers with guards" in {
    checkCode(
      q"""
         2 match {
           case 1 if 1 == 1 => 11
           case 2 if 0 == 1 => 12
           case 2 if 1 == 1 => 13
           case _           => 14
         }
       """, 13, Seq())
  }

  it should "match types on primitives" in {
    checkCode(
      q"""
         2.0 match {
           case x: Double => 1
           case x: Int    => 2
           case _         => 3
         }
       """, 1, Seq())
    checkCode(
      q"""
         2 match {
           case x: Double => 1
           case x: Int    => 2
           case _         => 3
         }
       """, 2, Seq())
    checkCode(
      q"""
         "test" match {
           case x: Double => 1
           case x: Int    => 2
           case _         => 3
         }
       """, 3, Seq())
  }

  it should "match types on interpreted classes" in {
    checkCode(
      q"""
         class A(val a1: Int, val a2: Double)
         class B(val b1: Int, val b2: Double)
         new A(1, 2.0) match {
           case x: A => 1
           case x: B => 2
         }
       """, 1, Seq())
    checkCode(
      q"""
         class A(val a1: Int, val a2: Double)
         class B(val b1: Int, val b2: Double)
         new B(1, 2.0) match {
           case x: A => 1
           case x: B => 2
         }
       """, 2, Seq())
  }

  ignore should "match types on interpreted inherited classes" in {
    checkCode(
      q"""
         class A(val a1: Int, val a2: Double)
         class B(val b1: Int, val b2: Double) extends A(b1, b2)
         new B(1, 2.0) match {
           case x: A => 1
           case x: B => 2
         }
       """, 1, Seq())
    checkCode(
      q"""
         class A(val a1: Int, val a2: Double)
         class B(val b1: Int, val b2: Double) extends A(b1, b2)
         new B(1, 2.0) match {
           case x: B => 2
           case x: A => 1
         }
       """, 2, Seq())
  }

  it should "match tuples" in {
    checkCode(
      q"""
         (1, 2.0) match {
           case (x, y) => x
           case _      => -1
         }
       """, 1, Seq())
    checkCode(
      q"""
         3 match {
           case (x, y) => x
           case _      => -1
         }
       """, -1, Seq())
  }

  it should "match binded tuples" in {
    checkCode(
      q"""
         (1, 2.0) match {
           case z @ (x, y) => z
           case _          => -1
         }
       """, (1, 2.0), Seq())
  }

  it should "match alternatives" in {
    checkCode(
      q"""
         (1, 1.0) match {
           case (1, 1.0) | (2, 2.0) => 1
           case (2, 1.0) | (1, 2.0) => 2
           case _                   => -1
         }
       """, 1, Seq())
    checkCode(
      q"""
         (1, 2.0) match {
           case (1, 1.0) | (2, 2.0) => 1
           case (2, 1.0) | (1, 2.0) => 2
           case _                   => -1
         }
       """, 2, Seq())
  }
}
