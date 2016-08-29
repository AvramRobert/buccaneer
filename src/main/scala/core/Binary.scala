package core

import scala.annotation.tailrec
import scalaz.{Applicative, ValidationNel, \/}
import scalaz.syntax.applicative._

// TODO: Test, perhaps next version
object Binary {

  def liftTree[A](a: A): Tree[A] = Node(a)

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

  def traverse[G[_], A, B](t: Tree[A])(f: A => G[B])(implicit ap: Applicative[G]): G[Tree[B]] = t match {
    case Node(v, lt, rt) => (f(v) |@| traverse(lt)(f) |@| traverse(rt)(f))(Node.apply)
    case Leaf => ap point Leaf
  }

  @tailrec def tailFold[A, B](branches: List[Tree[A]], b: B)(f: (B, A) => B): B = branches match {
    case Node(v, l, r) :: t => tailFold(l :: r :: t, f(b, v))(f)
    case Leaf :: t => tailFold(t, b)(f)
    case Nil => b
  }

  def foldl[A, B](t: Tree[A], b: B)(f: (B, A) => B): B = tailFold(t :: Nil, b)(f)
  def foldr[A, B](t: Tree[A], b: B)(f: (A, B) => B): B = tailFold(t :: Nil, b)((l, r) => f(r, l))

  def zipToList[A, B](tree: Tree[A], list: List[B]): List[(A, B)] = foldl(tree, (List.empty[(A, B)], list)) {
    case ((nl, h :: t), a) => ((a, h) :: nl, t)
  }._1.reverse

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

  def validate[A](t: Tree[A])(f: A => Throwable \/ A): ValidationNel[Throwable, Tree[A]] = {
    type λ[x] = ValidationNel[Throwable, x]
    def valid(a: A): λ[A] = f(a).validation.toValidationNel

    traverse[λ, A, A](t)(valid)
  }

  def validate2[A](t: List[A])(f: A => Throwable \/ A): ValidationNel[Throwable, List[A]] = {
    import scalaz.syntax.traverse._
    import scalaz.std.list._
    type λ[x] = ValidationNel[Throwable, x]
    def valid(a: A): λ[A] = f(a).validation.toValidationNel

    t.traverse[λ, A](valid)
  }

  def depth[A](t: Tree[A]): Int = foldl(t, 0)((i, _) => i + 1)

  implicit def treeSyntax[A](t: Tree[A]): TreeSyntax[A] = TreeSyntax(t)
}


object -< {
  def unapply[A](t: Tree[A]): Option[(A, Tree[A], Tree[A])] = t match {
    case Node(a, l, r) => Some((a, l, r))
    case Leaf => None
  }
}

sealed trait Tree[+A]

case class Node[A](value: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) extends Tree[A]

case object Leaf extends Tree[Nothing]

case class TreeSyntax[A](t: Tree[A]) {
  def affix(nb: Tree[A]): Tree[A] = Binary.affix(t, nb)

  def infix(nb: Tree[A]): Tree[A] = Binary.infix(t, nb)

  def affix(v: A): Tree[A] = affix(Binary.liftTree(v))
  def infix(v: A): Tree[A] = infix(Binary.liftTree(v))

  def map[B](f: A => B): Tree[B] = Binary.map(t)(f)

  def rootOf(p: A => Boolean): Boolean = Binary.rootOf(t)(p)

  def zipToList[B](list: List[B]): List[(A, B)] = Binary.zipToList(t, list)

  def depth: Int = Binary.depth(t)

  def zipL[B](list: List[B]): Tree[(A, B)] = Binary.zipL(t, list)

  def validate(f: A => Throwable \/ A): ValidationNel[Throwable, Tree[A]] = Binary.validate(t)(f)

  def rootOption: Option[A] = t match {
    case root -< (_, _) => Some(root)
    case _ => None
  }

  def filterL(f: A => Boolean): List[A] = Binary.foldr(t, List.empty[A]) {
    case (a, b) if f(a) => a :: b
    case (_, b) => b
  }

  def foldLeft[B](b: B)(f: (B, A) => B): B = Binary.foldl(t, b)(f)
}