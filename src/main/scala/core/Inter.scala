package core

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.applicative._

object Inter {
  def lift[A](a: => A): Inter[A] = Acc(a, End)

  def validate[A](inter: Inter[A])(f: A => Throwable \/ A): ValidationNel[Throwable, Inter[A]] = {
    type λ[x] = ValidationNel[Throwable, x]
    def valid(a: A): λ[A] = f(a).validation.toValidationNel

    inter.traverse[λ, A](valid)
  }
}

object :~ {
  def unapply[A](a: Inter[A]): Option[(A, Inter[A])] = a match {
    case Acc(p, r) => Some((p, r))
    case Brid(l, r) => unapply(l) match {
      case Some((az, i)) => Some((az, i affix r))
      case None => unapply(r)
    }
    case End => None
  }
}

// TODO: Isn't there a simpler algebra for representing the same idea?
// I could basically achieve the same behaviour by using a simple binary tree
sealed trait Inter[+A] {
  def affix[AA >: A](inter: Inter[AA]): Inter[AA] = this match {
    case Acc(p, r) => Acc(p, r affix inter)
    case Brid(l, r) => Brid(l, r affix inter)
    case End => inter
  }

  def infix[AA >: A](inter: Inter[AA]): Inter[AA] = this match {
    case Acc(p, End) => Brid(Acc(p, inter), End)
    case Acc(p, r) => Acc(p, r infix inter)
    case Brid(l, End) => Brid(l affix inter, End)
    case Brid(l, r) => Brid(l, r infix inter)
    case End => Brid(inter, End)
  }

  def map[B](f: A => B): Inter[B] = this match {
    case Acc(p, t) => Acc(f(p), t map f)
    case Brid(l, r) => Brid(l map f, r map f)
    case End => End
  }

  def foreach(f: A => Unit): Unit = {
    map(f)
  }

  def foldLeft[B](b: B)(f: (B, A) => B): B = this match {
    case Acc(p, t) => t.foldLeft(f(b, p))(f)
    case Brid(l, r) =>
      val nb = l.foldLeft(b)(f)
      r.foldLeft(nb)(f)
    case End => b
  }

  def traverse[G[_], B](f: A => G[B])(implicit ap: Applicative[G]): G[Inter[B]] = {
    @annotation.tailrec
    def go(rest: Inter[A], acc: G[Inter[B] => Inter[B]]): G[Inter[B]] = rest match {
      case Acc(p, r) =>
        val G = (f(p) |@| ap.point((b: B) => (i: Inter[B]) => Acc(b, i))) ((b, g) => g(b))
        go(r, (G |@| acc) (_ andThen _))
      case Brid(l, r) =>
        val GL = l.traverse(f)
        val G = GL map (nl => (i: Inter[B]) => Brid(nl, i))
        go(r, (G |@| acc) (_ andThen _))
      case End => acc map (_ (End))
    }
    go(this, ap.point(identity[Inter[B]] _))
  }

  def zipL[B](L: List[B]): Inter[(A, B)] = L match {
    case h :: t => this match {
      case Acc(p, r) => Acc((p, h), r zipL t)
      case Brid(l, r) =>
        val depth = l.depth
        Brid(l zipL (L take depth), r zipL (L drop depth))
      case End => End
    }
    case Nil => End
  }

  def exists(p: A => Boolean): Boolean = foldLeft(false) { (l, a) => p(a) || l }

  def startsAs(p: A => Boolean): Boolean = this match {
    case h :~ _ => p(h)
    case _ => false
  }

  def filterL(p: A => Boolean): List[A] = foldLeft(List.empty[A]) { (l, a) =>
    if (p(a)) a :: l
    else l
  }.reverse

  def depth: Int = foldLeft(0)((i, _) => i + 1)

  def head: A = this match {
    case h :~ _ => h
  }

  def headOption: Option[A] = this match {
    case h :~ _ => Some(h)
    case _ => None
  }
}

case class Acc[+A](point: A, rest: Inter[A]) extends Inter[A]

case class Brid[+A](left: Inter[A], right: Inter[A]) extends Inter[A]

case object End extends Inter[Nothing]