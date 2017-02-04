package core

import core.Read.Result

import scala.annotation.tailrec
import scalaz.Applicative
import scalaz.syntax.applicative._

// TODO: Test
object Tree {

  def lift[A](a: A): Tree[A] = Node(a)

  def affix[A](t: Tree[A], nb: Tree[A]): Tree[A] = t match {
    case Node(v, lt, rt) => Node(v, lt, affix(rt, nb))
    case Leaf => nb
  }

  def infix[A](t: Tree[A], nb: Tree[A]): Tree[A] = t match {
    case Node(v, lt, Leaf) => Node(v, affix(lt, nb))
    case Node(v, lt, rt) => Node(v, lt, infix(rt, nb))
    case Leaf => nb
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Node(v, lt, rt) => Node(f(v), map(lt)(f), map(rt)(f))
    case Leaf => Leaf
  }

  @tailrec def dropWhile[A](t: Tree[A])(p: A => Boolean): Tree[A] = t match {
    case Node(v, lt, rt) if p(v) => dropWhile(lt affix rt)(p)
    case node => node
  }

  def takeWhile[A](t: Tree[A])(p: A => Boolean): Tree[A] = t match {
    case Node(v, lt, rt) if p(v) =>
      val left = takeWhile(lt)(p)
      if(left.depth == lt.depth) Node(v, lt, takeWhile(rt)(p))
      else Node(v, left, Leaf)
    case Node(_, _, _) => Leaf
    case node => node
  }

  def traverse[G[_], A, B](t: Tree[A])(f: A => G[B])(implicit ap: Applicative[G]): G[Tree[B]] = t match {
    case Node(v, lt, rt) => (f(v) |@| traverse(lt)(f) |@| traverse(rt)(f)) (Node.apply)
    case Leaf => ap point Leaf
  }

  def sequence[G[_], A](t: Tree[G[A]])(implicit ap: Applicative[G]): G[Tree[A]] = traverse(t)(identity)

  @tailrec def tailFold[A, B](branches: List[Tree[A]], b: B)(f: (B, A) => B): B = branches match {
    case Node(v, l, r) :: t => tailFold(l :: r :: t, f(b, v))(f)
    case Leaf :: t => tailFold(t, b)(f)
    case Nil => b
  }

  def foldl[A, B](t: Tree[A], b: B)(f: (B, A) => B): B = tailFold(t :: Nil, b)(f)

  def foldr[A, B](t: Tree[A], b: B)(f: (A, B) => B): B = tailFold(t :: Nil, b)((l, r) => f(r, l))

  def zipL[A, B](tree: Tree[A], list: List[B]): Tree[(A, B)] = tree match {
    case r -< (tl, tr) => list match {
      case h :: t =>
        val d = depth(tl)
        Node((r, h), zipL(tl, t take d), zipL(tr, t drop d))
      case Nil => Leaf
    }
    case Leaf => Leaf
  }

  def rootOf[A](t: Tree[A])(p: A => Boolean): Boolean = t match {
    case root -< (_, _) => p(root)
    case _ => false
  }

  def validate[A](t: Tree[A])(f: A => Result[A]): Result[Tree[A]] = traverse(t)(f)

  def depth[A](t: Tree[A]): Int = foldl(t, 0)((i, _) => i + 1)

  def foreach[A](t: Tree[A])(f: A => Unit): Unit = foldl(t, ()) { (_, x) => f(x) }

  def serialise[A](t: Tree[A]): Vector[A] = foldl(t, Vector[A]())(_ :+ _)

  def zips[A, B](tree: Tree[A], list: List[B]): Tree[(A, Option[B])] = tree match {
    case v -< (l, r) => list match {
      case h :: t =>
        val depth = l.depth
        Node((v, Some(h)), zips(l, t.take(depth)), zips(r, t.drop(depth)))
      case Nil => Node((v, None), l.map(a => (a, None)), r.map(a => (a, None)))
    }
    case Leaf => Leaf
  }

  def string[A](t: Tree[A], sep: String)(f: A => String): String = foldl(t, "")((x, y) => x + sep + f(y))

  implicit def treeSyntax[A](t: Tree[A]): TreeSyntax[A] = TreeSyntax(t)
}


object -< {
  def unapply[A](t: Tree[A]): Option[(A, Tree[A], Tree[A])] = t match {
    case Node(a, l, r) => Some((a, l, r))
    case Leaf => None
  }
}

// It may be perhaps a good idea to turn this into a Vector[Denot] -> Some things would become MUCH simpler and faster
sealed trait Tree[+A]

case class Node[A](value: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) extends Tree[A]

case object Leaf extends Tree[Nothing]

case class TreeSyntax[A](t: Tree[A]) {
  def affix(nb: Tree[A]): Tree[A] = Tree.affix(t, nb)

  def infix(nb: Tree[A]): Tree[A] = Tree.infix(t, nb)

  def affix(v: A): Tree[A] = affix(Tree.lift(v))

  def infix(v: A): Tree[A] = infix(Tree.lift(v))

  def map[B](f: A => B): Tree[B] = Tree.map(t)(f)

  def foreach(f: A => Unit): Unit = Tree.foreach(t)(f)

  def rootOf(p: A => Boolean): Boolean = Tree.rootOf(t)(p)

  def depth: Int = Tree.depth(t)

  def zipL[B](list: List[B]): Tree[(A, B)] = Tree.zipL(t, list)

  def validate(f: A => Result[A]): Result[Tree[A]] = Tree.validate(t)(f)

  def rootOption: Option[A] = t match {
    case root -< (_, _) => Some(root)
    case _ => None
  }

  def filterL(f: A => Boolean): List[A] = Tree.foldr(t, List.empty[A]) {
    case (a, b) if f(a) => a :: b
    case (_, b) => b
  }.reverse

  def foldLeft[B](b: B)(f: (B, A) => B): B = Tree.foldl(t, b)(f)

  def dropWhile(p: A => Boolean): Tree[A] = Tree.dropWhile(t)(p)

  def takeWhile(p: A => Boolean): Tree[A] = Tree.takeWhile(t)(p)

  def serialise: Vector[A] = Tree.serialise(t)

  def string(sep: String)(f: A => String): String = Tree.string(t, sep)(f)

  def zips[B](list: List[B]): Tree[(A, Option[B])] = Tree.zips(t, list)
}