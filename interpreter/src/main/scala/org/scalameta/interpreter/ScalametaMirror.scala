package org.scalameta.interpreter

import scala.meta._

trait ScalametaMirror {
  def apply(name: Tree): Symbol
}

case object ScalametaMirrorImpl extends ScalametaMirror {
  override def apply(name: Tree): Symbol = sys.error("Unknown symbol")
}

object ScalametaMirror {
  val AnyEquals = Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Any")), Signature.Method("equals", "(Ljava/lang/Object;)Z"))
  val AnyHashcode = Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Any")), Signature.Method("hashCode", "()I"))
  val `Any==` = Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Any")), Signature.Method("==", "(I)Z"))
  val `Any!=` = Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Any")), Signature.Method("!=", "(I)Z"))
  val StringInterpolationS = Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("StringContext")), Signature.Method("s", "(Lscala/collection/Seq;)Ljava/lang/String;"))

  val emptySymbol = Symbol.Global(Symbol.None, Signature.Term("_empty_"))
  val A = Symbol.Global(emptySymbol, Signature.Type("A"))
  val B = Symbol.Global(emptySymbol, Signature.Type("B"))
  val OA = Symbol.Global(emptySymbol, Signature.Term("OA"))

  implicit class ScalametaSymbol(tree: Tree)(implicit mirror: ScalametaMirror) {
    def symbol: Symbol = {
      tree match {
        // Local
        case Term.Name("x") => Symbol.Local("x")
        case Term.Name("y") => Symbol.Local("y")
        case Term.Name("z") => Symbol.Local("z")
        case Term.Name("fooS") => Symbol.Global(emptySymbol, Signature.Method("fooS", "(Ljava/lang/String;)Ljava/lang/String;"))
        case Term.Name("fooI") => Symbol.Global(emptySymbol, Signature.Method("fooI", "(I)I"))
        case Term.Name("barS") => Symbol.Global(emptySymbol, Signature.Method("barS", "(Ljava/lang/String;)Ljava/lang/String;"))
        case Term.Name("barI") => Symbol.Global(emptySymbol, Signature.Method("barI", "(I)I"))
        // A
        case Type.Name("A") => A
        // case Ctor.Ref.Name("A") => Symbol.Global(A, Signature.Method("<init>", "(ID)V"))
        case Ctor.Ref.Name("A") => A // FIXME: Should point to constructor, not a type
        case Term.Name("a") => Symbol.Local("a")
        case Term.Name("a1") => Symbol.Global(A, Signature.TermParameter("a1"))
        case Term.Name("a2") => Symbol.Global(A, Signature.TermParameter("a2"))
        case Term.Name("ax") => Symbol.Global(A, Signature.Term("ax"))
        case Term.Name("ay") => Symbol.Global(A, Signature.Term("ay"))
        case Term.Name("fooAS") => Symbol.Global(A, Signature.Method("fooAS", "(Ljava/lang/String;)Ljava/lang/String;"))
        case Term.Name("fooAI") => Symbol.Global(A, Signature.Method("fooAI", "(I)I"))
        // B
        case Type.Name("B") => B
        // case Ctor.Ref.Name("B") => Symbol.Global(B, Signature.Method("<init>", "(ID)V"))
        case Ctor.Ref.Name("B") => B // FIXME: Should point to constructor, not a type
        case Term.Name("b") => Symbol.Local("b")
        case Term.Name("b1") => Symbol.Global(B, Signature.TermParameter("b1"))
        case Term.Name("b2") => Symbol.Global(B, Signature.TermParameter("b2"))
        // OA
        case Term.Name("OA") => OA
        case Term.Name("oax") => Symbol.Global(OA, Signature.Term("oax"))
        case Term.Name("fooOAI") => Symbol.Global(A, Signature.Method("fooOAI", "(I)I"))
        // Predef
        case Term.Name("s") => StringInterpolationS
        case Type.Name("Double") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Double"))
        case Type.Name("Int") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int"))
        case Term.Name("println") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Term("Predef")), Signature.Method("println", "(Ljava/lang/Object;)V"))
        case Term.Name("Seq") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Term("collection")), Signature.Term("Seq"))
        // Generic
        case Term.Name("equals") => AnyEquals
        case Term.Name("hashCode") => AnyHashcode
        case Term.Name("+") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int")), Signature.Method("+", "(I)I"))
        case Term.Name("-") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int")), Signature.Method("-", "(I)I"))
        case Term.Name("*") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int")), Signature.Method("*", "(I)I"))
        case Term.Name("/") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int")), Signature.Method("/", "(I)I"))
        case Term.Name("<") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int")), Signature.Method("<", "(I)I"))
        case Term.Name(">") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int")), Signature.Method(">", "(I)I"))
        case Term.Name("<=") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int")), Signature.Method("<=", "(I)I"))
        case Term.Name(">=") => Symbol.Global(Symbol.Global(Symbol.Global(Symbol.Global(Symbol.None, Signature.Term("_root_")), Signature.Term("scala")), Signature.Type("Int")), Signature.Method(">=", "(I)I"))
        case Term.Name("==") => `Any==`
        case Term.Name("!=") => `Any!=`
        // Internal
        case Term.Name("__interpreterMatchX__") => Symbol.Local("__interpreterMatchX__")
        case other => mirror(other)
      }
    }
  }
}
