package buccaneer

import buccaneer.Read.Result
import scala.annotation.tailrec
import scalaz.{Applicative, Equal, Traverse}
import scalaz.syntax.applicative._

object Tree {
  def apply[A](a: A): Tree[A] = Node(a)

  def empty[A]: Tree[A] = Leaf

  lazy implicit val traverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverseImpl[G[_], A, B](fa: Tree[A])(f: (A) => G[B])(implicit evidence$1: Applicative[G]) = fa traverse f
  }

  implicit def equal[A]: Equal[Tree[A]] = new Equal[Tree[A]] {
    override def equal(a1: Tree[A], a2: Tree[A]) = a1 == a2 //naive
  }
}

/** A binary tree, that is allowed to grow only in either a leftist or rightist fashion.
  *
  * @tparam A tree value type
  */
sealed trait Tree[+A] {

  /** Inserts a subtree into the current tree at the rightmost node.
    * Represents a rightist insert. Does not attempt to restructure `that`.
    *
    * @param that subtree to insert
    * @return a new tree containing the subtree `that` appended at the rightmost node
    */
  def affix[AA >: A](that: Tree[AA]): Tree[AA] = affixP(this, that)

  /** Inserts a subtree into the current tree at the leftmost node.
    * Represents a leftist insert. Does not attempt to restructure `that`.
    *
    * @param that subtree to insert
    * @return a new tree containing the subtree `that` appended at the leftmost node
    */
  def infix[AA >: A](that: Tree[AA]): Tree[AA] = infixP(this, that)

  /** Inserts a value into the current tree at the rightmost node.
    * Represents a rightist insert.
    *
    * @param value the value to insert
    * @return a new tree containing the value appended to the rightmost node
    */
  def affix[AA >: A](value: AA): Tree[AA] = affix(Tree(value))

  /** Inserts a value into the current tree at the leftmost node.
    * Represents a leftist insert.
    *
    * @param value the value to insert
    * @return a new tree containing the value appendended to the leftmost node
    */
  def infix[AA >: A](value: AA): Tree[AA] = infix(Tree(value))


  /** Applies a function on every element of the tree.
    *
    * @param f the function to apply
    * @tparam B the new type of the tree
    * @return a new tree, with the elements transformed by `f`
    */
  def map[B](f: A => B): Tree[B] = this match {
    case Node(v, lt, rt) => Node(f(v), lt map f, rt map f)
    case Leaf => Leaf
  }

  /** Calculates the amount of nodes in the tree.
    *
    * @return the amount of nodes in the tree.
    */
  def depth: Int = foldLeft(0)((n, _) => n + 1)

  /** Zips the elements of a list to those of the tree.
    * Traversal is pre-order. The resulting tree will
    * be as long as the shortest of either the list or the tree.
    *
    * @param list values to zip
    * @tparam B type of the zipped values
    * @return a tree of tuples, containing the (pre-ordered) values of the tree
    *         with some corresponding list value
    */
  def zipWithList[B](list: List[B]): Tree[(A, B)] = this match {
    case v -< (lt, rt) => list match {
      case h :: t =>
        val depth = lt.depth
        Node((v, h), lt.zipWithList(t take depth), rt.zipWithList(t drop depth))
      case Nil => Leaf
    }
    case Leaf => Leaf
  }

  /** Zips the elements of a list to those of the tree.
    * Traversal is pre-order. Contrary to `zipWithList`, this preserves the tree.
    * If the tree is larger than the list,
    * then the rest of the tree is zipped with `None`.
    *
    * @param list values to zip
    * @tparam B type of the zipped values
    * @return a tree of tuples, containing the (pre-ordered) original tree value and a possible
    *         value from the list
    */
  def zips[B](list: List[B]): Tree[(A, Option[B])] = this match {
    case v -< (lt, rt) => list match {
      case h :: t =>
        val depth = lt.depth
        Node((v, Some(h)), lt.zips(t.take(depth)), rt.zips(t.drop(depth)))
      case Nil => Node((v, None), lt.map(a => (a, None)), rt.map(a => (a, None)))
    }
    case Leaf => Leaf
  }

  /** Traverses the tree and applies a side-effecting function on every element.
    *
    * @param f side-effecting function
    */
  def foreach(f: A => Unit): Unit = foldLeft(()) { (_, x) => f(x) }

  /** Traverses the tree pre-order and preserves nodes as
    * long as the predicate asserts true.
    *
    * @param p predicate stating the condition for preservation
    * @return a new tree containing the nodes up until the predicate asserted false
    */
  def takeWhile[AA >: A](p: AA => Boolean): Tree[AA] = this match {
    case Node(v, lt, rt) if p(v) =>
      val left = lt takeWhile p
      if (left.depth == lt.depth) Node(v, lt, rt takeWhile p)
      else Node(v, left, Leaf)
    case Node(_, _, _) => Leaf
    case node => node
  }

  /** Traverses the tree pre-order and drops nodes as long as
    * the predicate asserts true.
    *
    * @param p predicate stating the condition for discarding
    * @return a new tree containing the nodes from the point where the predicated asserted false
    */
  def dropWhile[AA >: A](p: AA => Boolean): Tree[AA] = this match {
    case Node(v, lt, rt) if p(v) => (lt affix rt).dropWhile(p)
    case node => node
  }

  /** Traverses the tree pre-order and keeps the values of the tree for which
    * the predicate asserts true. This operation destroys the tree's structure
    * and, as such, it cannot be reconstructed. The kept values are thus
    * accumulated in a list.
    *
    * @param p predicate stating the condition for preservation
    * @return a list containing the preserved values
    */
  def filter[AA >: A](p: AA => Boolean): List[AA] = foldLeft(List.empty[AA]) {
    case (b, a) if p(a) => a :: b
    case (b, _) => b
  }.reverse

  /** A tail-recursive pre-order left fold over the tree.
    *
    * @param b seed value
    * @param f combination/tear-down function
    * @tparam B type of the resulting value
    * @return a new value, that is computed from the seed value
    *         in conjuction with `f` and every element of the tree
    */
  def foldLeft[B](b: B)(f: (B, A) => B): B = tailFold(List(this), b)(f)

  /** Predicative conjunction on all the elements of a tree.
    * Returns true of all applications of `p` are true.
    *
    * @param p predicate to check against the tree elements
    * @return predicative conjunction of all elements
    */
  def forall(p: A => Boolean): Boolean = foldLeft(true)((b, v) => b && p(v))

  /** Queries the root of the tree with a predicate if it is present.
    * Returns `false` otherwise.
    *
    * @param p predicate for querying the root
    * @return boolean result of the query; false if the empty tree.
    */
  def rootOf[AA >: A](p: AA => Boolean): Boolean = rootOption.exists(p)

  /** Returns the root of the tree speculatively.
    *
    * @return option that might contain the root value
    */
  def rootOption: Option[A] = this match {
    case root -< (_, _) => Some(root)
    case _ => None
  }

  /** Evaluates each value of the tree in some context and encases
    * the tree in that context.
    * This binary tree has a traversable functor.
    *
    * @param f context evaluation function
    * @tparam G evaluated context
    * @tparam B type of context
    * @return the complete tree evaluated in the given context
    */
  def traverse[G[_], B](f: A => G[B])(implicit A: Applicative[G]): G[Tree[B]] = this match {
    case Node(v, lt, rt) => (f(v) |@| lt.traverse(f) |@| rt.traverse(f)) (Node.apply)
    case Leaf => A point Leaf
  }

  /** Applies a validation function to each value of the tree, that can either
    * be successful or erroneous. Accumulates all errors that occur.
    * If the validation is successful, the result contains the tree itself.
    *
    * @param f validating function
    * @return a validation containing either a list of errors, or the
    *         tree itself
    */
  def validate[AA >: A](f: AA => Result[AA]): Result[Tree[AA]] = traverse(f)

  /** Serialises this tree to a `Vector` in pre-order.
    *
    * @return a vector containing the values of the tree in pre-order
    */
  def toVector: Vector[A] = foldLeft(Vector[A]())(_ :+ _)

  /** Function for stringifying the tree.
    * Given some separator and a way for converting the tree's values to `String`,
    * it creates a string that contains each value of the tree separated by the given
    * separator.
    *
    * @param sep  a separator for the tree's values
    * @param show function that converts the tree's values to `String`
    * @return string containing each value of the tree separated by the given separator
    */
  def string(sep: String)(show: A => String): String = {
    val seps = Stream.continually(sep).take(depth - 1).toList
    zips(seps).foldLeft("") {
      case (full, (a, Some(s))) => full + show(a) + s
      case (full, (a, _)) => full + show(a)
    }
  }

  private final def affixP[X](ths: Tree[X], tht: Tree[X]): Tree[X] = ths match {
    case Node(v, lt, rt) => Node(v, lt, affixP(rt, tht))
    case Leaf => tht
  }

  private final def infixP[X](ths: Tree[X], tht: Tree[X]): Tree[X] = ths match {
    case Node(v, lt, Leaf) => Node(v, infixP(lt, tht))
    case Node(v, lt, rt) => Node(v, lt, infixP(rt, tht))
    case Leaf => tht
  }

  @tailrec private final def tailFold[AA >: A, B](branches: List[Tree[AA]], b: B)(f: (B, AA) => B): B = branches match {
    case Node(v, l, r) :: t => tailFold(l :: r :: t, f(b, v))(f)
    case Leaf :: t => tailFold(t, b)(f)
    case Nil => b
  }
}

/** A binary tree node, containing a value, a left and right branch.
  *
  * @param value the value at the given node
  * @param left  left subtree
  * @param right right subtree
  * @tparam A tree value type
  */
case class Node[A](value: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) extends Tree[A]

/** The empty binary tree.
  */
case object Leaf extends Tree[Nothing]

object -< {
  def unapply[A](t: Tree[A]): Option[(A, Tree[A], Tree[A])] = t match {
    case Node(a, l, r) => Some((a, l, r))
    case Leaf => None
  }
}