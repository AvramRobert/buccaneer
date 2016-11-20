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
  def id(symbol: Sym): Identifier = Identifier(symbol)
  def typing[A](proof: Reified[A]): Typing[A] = Typing(proof)
  def typedId[A](symbol: Sym, proof: Reified[A]): TypedIdentifier[A] = TypedIdentifier(symbol, proof)
}

sealed trait Denot {
  def isTyped: Boolean = this match {
    case Typing(_) | TypedIdentifier(_, _) => true
    case _ => false
  }
}
case class Identifier(symbol: Sym) extends Denot
case class Typing[A](proof: Reified[A]) extends Denot
case class TypedIdentifier[A](symbol: Sym, proof: Reified[A]) extends Denot