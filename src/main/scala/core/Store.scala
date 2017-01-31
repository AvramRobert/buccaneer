package core

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Command needs to widen the type to Any, as the preceding types of ${A} and current returned type are not equal. \n" +
    "To do this, please use `+>`, instead of `+`.")
private[core]
sealed trait AddW[M[_], A] {
  def add[B](m: M[A])(itm: Cmd[B]): M[A]
}

private[core]
sealed trait Add[M[_], A] {
  def add(m: M[A])(itm: Cmd[A]): M[A]
}

private[core]
sealed trait Get[M[_], A] {
  def get(m: M[A])(key: Tree[Denot]): Option[Cmd[A]]
}

private[core]
sealed trait Keys[M[_], A] {
  def keySey(m: M[A]): Set[Tree[Denot]]
}
// TODO: Abstract this further?
object Store {
  type MapT[A] = Map[Tree[Denot], Cmd[A]]

  def empty[A] = Store[MapT, A](Map.empty[Tree[Denot], Cmd[A]])
  def widen[A](store: Store[MapT, A]): Store[MapT, Any] = store.underlying.foldLeft(empty[Any])((a, b) => a +> b._2)

  implicit def addTC[A]: Add[MapT, A] = new Add[MapT, A] {
    override def add(m: MapT[A])(itm: Cmd[A]): MapT[A] = m + (itm.syntax -> itm)
  }

  implicit def addWTC: AddW[MapT, Any] = new AddW[MapT, Any] {
    override def add[B](m: MapT[Any])(itm: Cmd[B]): MapT[Any] = m + (itm.syntax -> itm.asInstanceOf[Cmd[Any]])
  }

  implicit def getTC[A]: Get[MapT, A] = new Get[MapT, A] {
    override def get(m: MapT[A])(key: Tree[Denot]): Option[Cmd[A]] = m get key
  }

  implicit def keysTC[A]: Keys[MapT, A] = new Keys[MapT, A] {
    override def keySey(m: MapT[A]): Set[Tree[Denot]] = m.keySet
  }
}

private[core]
case class Store[M[_], A](private[core] val underlying: M[A]) {
  def keySet(implicit keys: Keys[M, A]) = keys.keySey(underlying)

  def +(r: Cmd[A])(implicit plus: Add[M, A]) = Store(plus.add(underlying)(r))

  def +>[B](r: Cmd[B])(implicit plusW: AddW[M, A]) = Store(plusW.add(underlying)(r))

  def get(key: Tree[Denot])(implicit read: Get[M, A]): Option[Cmd[A]] = read.get(underlying)(key)
}