package core

import core.Store._

//TODO: Perhaps it might be a good idea to invert priority. Untyped should be first and typed second

object Cli {
  def command[A](label: String, desc: String = ""): TypedCli[A] = (cmd andThen typedCli[A]) (label, desc)

  def typedCli[A]: P0 => TypedCli[A] = TypedCli(_, Store.empty[A])

  def untypedCli: P0 => UntypedCli = UntypedCli(_, Store.empty[Any])

  def rebase[A](t: TypedCli[A]): P0 => TypedCli[A] = TypedCli(_, t.store)

  def rebase(u: UntypedCli): P0 => UntypedCli = UntypedCli(_, u.store)

  def cmd: ((String, String)) => P0 = t => Command(t._1, t._2)
}

sealed trait Cli[A] {
  def store: Store[MapT, A]
}

case class TypedCli[A](p0: P0, store: Store[MapT, A]) extends Cli[A] {
  def command(label: String, desc: String = "") = (Cli.cmd andThen Cli.rebase(this)) (label, desc)

  def child(f: P0 => Runner[A]) = TypedCli(p0, store + f(p0))

  def childT[B](f: P0 => Runner[B]) = {
    val untyped = Store.widen(store)
    UntypedCli(p0, untyped +> f(p0))
  }
}

case class UntypedCli(p0: P0, store: Store[MapT, Any]) extends Cli[Any] {
  def command(label: String, desc: String = "") = (Cli.cmd andThen Cli.rebase(this)) (label, desc)

  def childT[B](f: P0 => Runner[B]) = UntypedCli(p0, store +> f(p0))
}