package core

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Command needs to widen the type to Any, as the preceding types of ${A} and current returned type are not equal. \n" +
    "To do this, please use `+>`, instead of `+`")
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

object Store {
  type SMap[A] = Map[Tree[Denot], Cmd[A]]

  def empty[A] = Store[SMap, A](Map.empty[Tree[Denot], Cmd[A]])
  def widen[A](store: Store[SMap, A]): Store[SMap, Any] = store.underlying.foldLeft(empty[Any])((a, b) => a +> b._2)

  implicit def addTC[A]: Add[SMap, A] = new Add[SMap, A] {
    override def add(m: SMap[A])(itm: Cmd[A]): SMap[A] = m + (itm.syntax -> itm)
  }

  implicit def addWTC: AddW[SMap, Any] = new AddW[SMap, Any] {
    override def add[B](m: SMap[Any])(itm: Cmd[B]): SMap[Any] = m + (itm.syntax -> itm.asInstanceOf[Cmd[Any]])
  }

  implicit def getTC[A]: Get[SMap, A] = new Get[SMap, A] {
    override def get(m: SMap[A])(key: Tree[Denot]): Option[Cmd[A]] = m get key
  }

  implicit def keysTC[A]: Keys[SMap, A] = new Keys[SMap, A] {
    override def keySey(m: SMap[A]): Set[Tree[Denot]] = m.keySet
  }
}

private[core]
case class Store[M[_], A](private[core] val underlying: M[A]) {
  def keySet(implicit keys: Keys[M, A]) = keys.keySey(underlying)

  def +(r: Cmd[A])(implicit plus: Add[M, A]) = Store(plus.add(underlying)(r))

  def +>[B](r: Cmd[B])(implicit plusW: AddW[M, A]) = Store(plusW.add(underlying)(r))

  def get(key: Tree[Denot])(implicit read: Get[M, A]): Option[Cmd[A]] = read.get(underlying)(key)
}