package org.scalameta.interpreter.integration

import org.scalameta.interpreter.ScalametaMirror.emptySymbol
import org.scalameta.interpreter._

import scala.meta._

class ScalaCourseraWeek2 extends ScalametaInterpreterSpec {
  val FunSets = Symbol.Global(emptySymbol, Signature.Term("FunSets"))

  implicit val mirror: ScalametaMirror = {
    case Term.Name("FunSets")      => FunSets
    case Term.Name("elem")         => Symbol.Local("elem")
    case Term.Name("s")            => Symbol.Local("s")
    case Term.Name("e")            => Symbol.Local("e")
    case Term.Name("t")            => Symbol.Local("t")
    case Term.Name("p")            => Symbol.Local("p")
    case Term.Name("f")            => Symbol.Local("f")
    case Term.Name("contains")     => Symbol.Global(FunSets, Signature.Method("contains", "(Lscala/Function1;I)Z"))
    case Term.Name("singletonSet") => Symbol.Global(FunSets, Signature.Method("singletonSet", "(I)Lscala/Function1;"))
    case Term.Name("union")        => Symbol.Global(FunSets, Signature.Method("union", "(Lscala/Function1;Lscala/Function1;)Lscala/Function1;"))
    case Term.Name("intersect")    => Symbol.Global(FunSets, Signature.Method("intersect", "(Lscala/Function1;Lscala/Function1;)Lscala/Function1;"))
    case Term.Name("diff")         => Symbol.Global(FunSets, Signature.Method("diff", "(Lscala/Function1;Lscala/Function1;)Lscala/Function1;"))
    case Term.Name("filter")       => Symbol.Global(FunSets, Signature.Method("filter", "(Lscala/Function1;Lscala/Function1;)Lscala/Function1;"))
    case Term.Name("bound")        => Symbol.Global(FunSets, Signature.Term("bound"))
    case Term.Name("forall")       => Symbol.Global(FunSets, Signature.Method("forall", "(Lscala/Function1;Lscala/Function1;)Z"))
    case Term.Name("iter")         => Symbol.Local("iter")
    case Term.Name("exists")       => Symbol.Global(FunSets, Signature.Method("exists", "(Lscala/Function1;Lscala/Function1;)Z"))
    case Term.Name("map")          => Symbol.Global(FunSets, Signature.Method("iter", "(Lscala/Function1;Lscala/Function1;)Lscala/Function1;"))
    case Term.Name("toString")     => Symbol.Global(FunSets, Signature.Method("toString", "(Lscala/Function1;)Ljava/lang/String;"))
    case Term.Name("printSet")     => Symbol.Global(FunSets, Signature.Method("printSet", "(Lscala/Function1;)V"))
    case Term.Name("s1")            => Symbol.Local("s1")
    case Term.Name("s2")            => Symbol.Local("s2")
    case Term.Name("s3")            => Symbol.Local("s3")
  }

  it should "be able to declare FunSets" in {
    checkCode(q"""
         /**
          * 2. Purely Functional Sets.
          */
         object FunSets {
           /**
            * We represent a set by its characteristic function, i.e.
            * its `contains` predicate.
            */
           type Set = Int => Boolean
         
           /*
            * Indicates whether a set contains a given element.
            */
           def contains(s: Set, elem: Int): Boolean = s(elem)
         
           /*
            * Returns the set of the one given element.
            */
           def singletonSet(elem: Int): Set = x => (x == elem)
         
           /*
            * Returns the union of the two given sets,
            * the sets of all elements that are in either `s` or `t`.
            */
           def union(s: Set, t: Set): Set = (e: Int) => s(e) || t(e)
         
           /*
            * Returns the intersection of the two given sets,
            * the set of all elements that are both in `s` or `t`.
            */
           def intersect(s: Set, t: Set): Set = (e: Int) => s(e) && t(e)
         
           /*
            * Returns the difference of the two given sets,
            * the set of all elements of `s` that are not in `t`.
            */
           def diff(s: Set, t: Set): Set = (e: Int) => s(e) && !t(e)
         
           /*
            * Returns the subset of `s` for which `p` holds.
            */
           def filter(s: Set, p: Int => Boolean): Set = (e: Int) => s(e) && p(e)
         
           /*
            * The bounds for `forall` and `exists` are +/- 1000.
            */
           val bound = 1000
         
           /*
            * Returns whether all bounded integers within `s` satisfy `p`.
            */
           def forall(s: Set, p: Int => Boolean): Boolean = {
             def iter(a: Int): Boolean = {
               if (a > bound) true
               else if (contains(s, a) && !p(a)) false
               else iter(a+1)
             }
             iter(-bound)
           }
         
           /*
            * Returns whether there exists a bounded integer within `s`
            * that satisfies `p`.
            */
           def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (x => !p(x)))
         
           /*
            * Returns a set transformed by applying `f` to each element of `s`.
            */
           def map(s: Set, f: Int => Int): Set = (e:Int) => exists(s, (x => f(x) == e))
         
           /*
            * Displays the contents of a set
            */
           def toString(s: Set): String = {
             val xs = for (i <- -bound to bound if contains(s, i)) yield i
             xs.mkString("{", ",", "}")
           }
         
           /*
            * Prints the contents of a set on the console.
            */
           def printSet(s: Set) {
             println(toString(s))
           }
         }
         
         val s1 = FunSets.singletonSet(1)
         val s2 = FunSets.singletonSet(2)
         val s3 = FunSets.singletonSet(3)

         FunSets.contains(x => true, 100) && 
           FunSets.contains(s1, 1) &&
           !FunSets.contains(s1, 2) &&
           FunSets.contains(FunSets.union(s1, s2), 1) &&
           FunSets.contains(FunSets.intersect(FunSets.union(s1, s2), FunSets.union(s1, s3)), 1) &&
           !FunSets.contains(FunSets.intersect(FunSets.union(s1, s2), FunSets.union(s1, s3)), 2) &&
           !FunSets.contains(FunSets.intersect(FunSets.union(s1, s2), FunSets.union(s1, s3)), 3)
       """, true, Seq())
  }
}
