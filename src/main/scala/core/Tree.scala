package core

import core.Read.Result

import scala.annotation.tailrec
import scalaz.Applicative
import scalaz.syntax.applicative._

// TODO: Test
object Tree {
  def apply[A](a: A): Tree[A] = Node(a)
}
// It may be perhaps a good idea to turn this into a Vector[Denot] -> Some things would become MUCH simpler and faster
sealed trait Tree[+A] {
  def affix[AA >: A](that: Tree[AA]): Tree[AA] = affixP(this, that)

  def infix[AA >: A](that: Tree[AA]): Tree[AA] = infixP(this, that)

  def affix[AA >: A](value: AA): Tree[AA] = affix(Tree(value))

  def infix[AA >: A](value: AA): Tree[AA] = infix(Tree(value))

  // can be implemented in terms of traverse
  def map[B](f: A => B): Tree[B] = this match {
    case Node(v, lt, rt) => Node(f(v), lt map f, rt map f)
    case Leaf => Leaf
  }

  def depth: Int = foldLeft(0)((n, _) => n + 1)

  def zipWithList[B](list: List[B]): Tree[(A, B)] = this match {
    case v -< (lt, rt) => list match {
      case h :: t =>
        val depth = lt.depth
        Node((v, h), lt.zipWithList(t take depth), rt.zipWithList(t drop depth))
      case Nil => Leaf
    }
    case Leaf => Leaf
  }

  def zips[B](list: List[B]): Tree[(A, Option[B])] = this match {
    case v -< (lt, rt) => list match {
      case h :: t =>
        val depth = lt.depth
        Node((v, Some(h)), lt.zips(t.take(depth)), rt.zips(t.drop(depth)))
      case Nil => Node((v, None), lt.map(a => (a, None)), rt.map(a => (a, None)))
    }
    case Leaf => Leaf
  }

  def foreach(f: A => Unit): Unit = foldLeft(()) { (_, x) => f(x) }

  def takeWhile[AA >: A](p: AA => Boolean): Tree[AA] = this match {
    case Node(v, lt, rt) if p(v) =>
      val left = lt takeWhile p
      if (left.depth == lt.depth) Node(v, lt, rt takeWhile p)
      else Node(v, left, Leaf)
    case Node(_, _, _) => Leaf
    case node => node
  }

  def dropWhile[AA >: A](p: AA => Boolean): Tree[AA] = this match {
    case Node(v, lt, rt) if p(v) => (lt affix rt).dropWhile(p)
    case node => node
  }

  def filter[AA >: A](p: AA => Boolean): List[AA] = foldLeft(List.empty[AA]) {
    case (b, a) if p(a) => a :: b
    case (b, _) => b
  }.reverse

  def foldLeft[B](b: B)(f: (B, A) => B): B = tailFold(List(this), b)(f)

  def rootOf[AA >: A](p: AA => Boolean): Boolean = rootOption.exists(p)

  def rootOption: Option[A] = this match {
    case root -< (_, _) => Some(root)
    case _ => None
  }

  def traverse[G[_], B](f: A => G[B])(implicit A: Applicative[G]): G[Tree[B]] = this match {
    case Node(v, lt, rt) => (f(v) |@| lt.traverse(f) |@| rt.traverse(f)) (Node.apply)
    case Leaf => A point Leaf
  }

  def validate[AA >: A](f: AA => Result[AA]): Result[Tree[AA]] = traverse(f)

  def toVector: Vector[A] = foldLeft(Vector[A]())(_ :+ _)

  def string(sep: String)(show: A => String): String = foldLeft("")((x, y) => x + sep + show(y))


  private final def affixP[X](ths: Tree[X], tht: Tree[X]): Tree[X] = ths match {
    case Node(v, lt, rt) => Node(v, lt, affixP(rt, tht))
    case Leaf => tht
  }

  private final def infixP[X](ths: Tree[X], tht: Tree[X]): Tree[X] = ths match {
    case Node(v, lt, Leaf) => Node(v, affixP(lt, tht))
    case Node(v, lt, rt) => Node(v, lt, infixP(rt, tht))
    case Leaf => tht
  }

  @tailrec private final def tailFold[AA >: A, B](branches: List[Tree[AA]], b: B)(f: (B, AA) => B): B = branches match {
    case Node(v, l, r) :: t => tailFold(l :: r :: t, f(b, v))(f)
    case Leaf :: t => tailFold(t, b)(f)
    case Nil => b
  }
}

case class Node[A](value: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) extends Tree[A]

case object Leaf extends Tree[Nothing]

object -< {
  def unapply[A](t: Tree[A]): Option[(A, Tree[A], Tree[A])] = t match {
    case Node(a, l, r) => Some((a, l, r))
    case Leaf => None
  }
}