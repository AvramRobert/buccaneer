package core

import core.Store._

//TODO: Perhaps it might be a good idea to invert priority. Untyped should be first and typed second

object Cli {
  def command[A](label: String, desc: String = ""): TypedCli[A] = (cmd andThen typedCli[A]) (label, desc)

  def typedCli[A]: Cmd0 => TypedCli[A] = TypedCli(_, Store.empty[A])

  def untypedCli: Cmd0 => UntypedCli = UntypedCli(_, Store.empty[Any])

  def rebase[A](t: TypedCli[A]): Cmd0 => TypedCli[A] = TypedCli(_, t.store)

  def rebase(u: UntypedCli): Cmd0 => UntypedCli = UntypedCli(_, u.store)

  def cmd: ((String, String)) => Cmd0 = t => Command(t._1, t._2)
}

sealed trait Cli[A] {
  def store: Store[MapT, A]
}

case class TypedCli[A](cmd0: Cmd0, store: Store[MapT, A]) extends Cli[A] {
  def command(label: String, desc: String = "") = (Cli.cmd andThen Cli.rebase(this)) (label, desc)

  def child(f: Cmd0 => Cmd[A]) = TypedCli(cmd0, store + f(cmd0))

  def childT[B](f: Cmd0 => Cmd[B]) = {
    val untyped = Store.widen(store)
    UntypedCli(cmd0, untyped +> f(cmd0))
  }
}

case class UntypedCli(cmd0: Cmd0, store: Store[MapT, Any]) extends Cli[Any] {
  def command(label: String, desc: String = "") = (Cli.cmd andThen Cli.rebase(this)) (label, desc)

  def childT[B](f: Cmd0 => Cmd[B]) = UntypedCli(cmd0, store +> f(cmd0))
}