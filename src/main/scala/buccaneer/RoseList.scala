package buccaneer

import scala.annotation.tailrec

// FIXME: Wouldn't actually be a better idea to use `RoseList` up until the interpretation step
// A RoseList would be stored in the Cli and only unbundled when an input is given and interpreted
// This would thus allow me to do transformations when the input is known, instead of unbundling blindly beforehand
// This would also make, for example, varargs possible, whereupon I would unbundle the remaining input when interpreting
object RoseList { self =>
  def empty[A]: RoseList[A] = Stem

  def prepend[A](roseList: RoseList[A], a: A): RoseList[A] = Petal(a, roseList)

  def prependMany[A](roseList: RoseList[A], as: A*): RoseList[A] = Bundle(as.toList, roseList)

  def grow[A](roseList: RoseList[A], a: A): RoseList[A] = roseList match {
    case Bundle(as, link) => Bundle(a :: as, link)
    case Petal(aa, link) => Bundle(a :: aa :: Nil, link)
    case Stem => prepend(roseList, a)
  }

  def reverse[A](roseList: RoseList[A]): RoseList[A] = {
    @tailrec def go(rem: RoseList[A], acc: RoseList[A] = Stem): RoseList[A] = rem match {
      case Petal(a, tail) => go(tail, prepend(acc, a))
      case Bundle(as, tail) => go(tail, prependMany(acc, as: _*))
      case Stem => acc
    }

    go(roseList)
  }

  def unbundle[A](roseList: RoseList[A]): Set[Vector[A]] = {
    def go(rem: RoseList[A], cur: Vector[A] = Vector.empty, total: Set[Vector[A]] = Set.empty): Set[Vector[A]] = rem match {
      case Petal(a, t) => go(t, cur :+ a, total)
      case Bundle(as, t) =>
        total ++ as.permutations.toSet.flatMap { perm: List[A] => go(t, cur ++ perm.toVector) }
      case Stem => total + cur
    }

    go(roseList)
  }

  implicit class RoseListOps[A](roseList: RoseList[A]) {
    def prepend(a: A): RoseList[A] = self.prepend(roseList, a)

    def prependMany(as: A*): RoseList[A] = self.prependMany(roseList, as: _*)

    def grow(a: A): RoseList[A] = self.grow(roseList, a)

    def unbundle: Set[Vector[A]] = self.unbundle(self.reverse(roseList))
  }
}

sealed trait RoseList[+A]

case class Petal[A](head: A, tail: RoseList[A]) extends RoseList[A]

case class Bundle[A](head: List[A], tail: RoseList[A]) extends RoseList[A]

case object Stem extends RoseList[Nothing]