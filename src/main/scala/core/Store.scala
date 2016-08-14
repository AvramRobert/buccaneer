package core

import scala.annotation.implicitNotFound
import scalaz.Functor

@implicitNotFound(
  "Command widens the type to Any, as the preceding types of ${A} and current returned type are not equal. \n" +
    "To avoid this, please use `+>`, instead of `+` from the beginning.")
private[core]
sealed trait AddW[M[_], A] {
  def add[B](m: M[A])(itm: Runner[B]): M[A]
}

private[core]
sealed trait Add[M[_], A] {
  def add(m: M[A])(itm: Runner[A]): M[A]
}

private[core]
sealed trait Get[M[_], A] {
  def get(m: M[A])(key: Inter[Sym]): Option[Runner[A]]
}

private[core]
sealed trait Keys[M[_], A] {
  def keySey(m: M[A]): Set[Inter[Sym]]
}
// TODO: Abstract this further?
object Store {
  type MapT[A] = Map[Inter[Sym], Runner[A]]

  def empty[A] = Store[MapT, A](Map.empty[Inter[Sym], Runner[A]])
  def widen[A](store: Store[MapT, A]): Store[MapT, Any] = store.underlying.foldLeft(empty[Any])((a, b) => a +> b._2)

  implicit def addTC[A]: Add[MapT, A] = new Add[MapT, A] {
    override def add(m: MapT[A])(itm: Runner[A]): MapT[A] = m + (itm.syntax -> itm)
  }

  implicit def addWTC: AddW[MapT, Any] = new AddW[MapT, Any] {
    override def add[B](m: MapT[Any])(itm: Runner[B]): MapT[Any] = m + (itm.syntax -> itm.asInstanceOf[Runner[Any]])
  }

  implicit def getTC[A]: Get[MapT, A] = new Get[MapT, A] {
    override def get(m: MapT[A])(key: Inter[Sym]): Option[Runner[A]] = m get key
  }

  implicit def keysTC[A]: Keys[MapT, A] = new Keys[MapT, A] {
    override def keySey(m: MapT[A]): Set[Inter[Sym]] = m.keySet
  }
}

private[core]
case class Store[M[_], A](private[core] val underlying: M[A]) {
  def keySet(implicit keys: Keys[M, A]) = keys.keySey(underlying)

  def +(r: Runner[A])(implicit plus: Add[M, A]) = Store(plus.add(underlying)(r))

  def +>[B](r: Runner[B])(implicit plusW: AddW[M, A]) = Store(plusW.add(underlying)(r))

  def get(key: Inter[Sym])(implicit read: Get[M, A]): Option[Runner[A]] = read.get(underlying)(key)
}