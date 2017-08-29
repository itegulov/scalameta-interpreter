package org.scalameta.interpreter.integration

import org.scalameta.interpreter.ScalametaMirror.emptySymbol
import org.scalameta.interpreter._

import scala.meta._

class ScalaCourseraWeek1 extends ScalametaInterpreterSpec {
  implicit val mirror: ScalametaMirror = {
    // Pascal
    case Term.Name("c")      => Symbol.Local("c")
    case Term.Name("r")      => Symbol.Local("r")
    case Term.Name("pascal") => Symbol.Global(emptySymbol, Signature.Method("pascal", "(II)I"))
    // Balance
    case Term.Name("chars")   => Symbol.Local("chars")
    case Term.Name("count")   => Symbol.Local("count")
    case Term.Name("cs")      => Symbol.Local("cs")
    case Term.Name("balance") => Symbol.Global(emptySymbol, Signature.Method("balance", "(Lscala/collection/immutable/List;I)Z"))
    // Count change
    case Term.Name("money")       => Symbol.Local("money")
    case Term.Name("coins")       => Symbol.Local("coins")
    case Term.Name("m")           => Symbol.Local("m")
    case Term.Name("countChange") => Symbol.Global(emptySymbol, Signature.Method("countChange", "(ILscala/collection/immutable/List;)I"))
  }

  it should "work on pascal triangle" in {
    checkCode(q"""
         def pascal(c: Int, r: Int): Int = (c, r) match {
           case (0, _)             => 1
           case (c, r) if c == r   => 1
           case (c, r)             => pascal(c, r - 1) + pascal(c - 1, r - 1)
         }

         pascal(5, 10)
       """, 252, Seq())
  }

  it should "work on brackets balance" in {
    checkCode(q"""
        def balance(chars: List[Char], count: Int = 0): Boolean = (chars, count) match {
          case (cs, 0) if cs.isEmpty => true
          case (cs, _) if cs.isEmpty => false
          case (cs, c) =>
            cs.head match {
              case '('          => balance(cs.tail, c + 1)
              case ')' if c > 0 => balance(cs.tail, c - 1)
              case ')'          => false
              case _            => balance(cs.tail, c)
            }
        }

        balance(augmentString("(if (zero? x) max (/ 1 x))").toList)
      """, true, Seq())
  }

  it should "work on count change" in {
    checkCode(q"""
        def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
          case (0, _)                => 1
          case (m, _) if m < 0       => 0
          case (_, cs) if cs.isEmpty => 0
          case (m, cs)               => countChange(m - cs.head, cs) + countChange(m, cs.tail)
        }
        countChange(100, List(5, 10, 20, 50, 100))
      """, 50, Seq())
  }
}
