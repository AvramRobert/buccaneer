package core

import scala.annotation.tailrec

object Sym {
  implicit def conversion(s: String): Sym = Label(s)
}

sealed trait Sym {
  def | (that: String): Alternative = this match {
    case Label(v) => Alternative(v, Label(that))
    case Alternative(v, alt) => Alternative(v, alt | that)
  }

  @tailrec final def find(p: String => Boolean): Option[Label] = this match {
    case x@Label(value) if p(value) => Some(x)
    case Alternative(value, _) if p(value) => Some(Label(value))
    case Alternative(_, symbol) => symbol find p
    case _ => None
  }

  def show: String = this match {
    case Label(value) => value
    case Alternative(value, symbol) => s"$value | ${symbol.show} "
  }
}
case class Label(value: String) extends Sym
case class Alternative(value: String, alt: Sym) extends Sym

object Denot {
  def id(symbol: Sym, docs: Docs = Docs.empty): Identifier = Identifier(symbol, docs)
  def typing[A](proof: Reified[A], docs: Docs = Docs.empty): Typing[A] = Typing(proof, docs)
  def typedId[A](symbol: Sym, proof: Reified[A], docs: Docs = Docs.empty): TypedIdentifier[A] = TypedIdentifier(symbol, proof, docs)
}

sealed trait Denot {
  def isTyped: Boolean = this match {
    case Typing(_, _) | TypedIdentifier(_, _, _) => true
    case _ => false
  }

  def docs: Docs

  def mapDocs(f: Docs => Docs): Denot = this match {
    case id @ Identifier(_, docs) => id.copy(docs = f(docs))
    case typing @ Typing(_, docs) => typing.copy(docs = f(docs))
    case typedId @ TypedIdentifier(_, _, docs) => typedId.copy(docs = f(docs))
  }
}

case class Identifier(symbol: Sym, docs: Docs) extends Denot
case class Typing[A](proof: Reified[A], docs: Docs) extends Denot
case class TypedIdentifier[A](symbol: Sym, proof: Reified[A], docs: Docs) extends Denot

object Docs {
  def empty: Docs = Docs("")
}

case class Docs(msg: String) {
  def mapMsg(f: String => String): Docs = copy(msg = f(msg))
}