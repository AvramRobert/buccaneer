package core

import core.Store._

//TODO: Perhaps it might be a good idea to invert priority. Untyped should be first and typed second

object Cli {
  def command[A](label: String)(f: P0 => Runner[A]): TypedCli[A] = insert(empty[A])(label, f)

  def insert[A](m: Store[MapT, A])(label: String, f: P0 => Runner[A]): TypedCli[A] = {
    val p0 = Command(label)
    TypedCli(p0, m + f(p0))
  }

  def insert[A](m: Store[MapT, Any])(label: String, f: P0 => Runner[A]): UntypedCli = {
    val p0 = Command(label)
    UntypedCli(p0, m +> f(p0))
  }
}

sealed trait Cli[A] {
  def store: Store[MapT, A]
}
case class TypedCli[A](p0: P0, store: Store[MapT, A]) extends Cli[A] {
  def command(label: String)(f: P0 => Runner[A]) = Cli.insert(store)(label, f)
  def commandT[B](label: String)(f: P0 => Runner[B]) = {
    val untyped = Store.widen(store)
    Cli.insert(untyped)(label, f)
  }
  def child(f: P0 => Runner[A]) = TypedCli(p0, store + f(p0))
  def childT[B](f: P0 => Runner[B]) = {
    val untyped = Store.widen(store)
    UntypedCli(p0, untyped +> f(p0))
  }
}
case class UntypedCli(p0: P0, store: Store[MapT, Any]) extends Cli[Any] {
  def commandT[B](label: String)(f: P0 => Runner[B]) = Cli.insert(store)(label, f)
  def childT[B](f: P0 => Runner[B]) = UntypedCli(p0, store +> f(p0))
}