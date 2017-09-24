package buccaneer

import scalaz.NonEmptyList

object Read {
  def apply[A](typeName: String)(f: String => Result[A]): Read[A] = new Read[A] {
    override def apply(a: String): Result[A] = f(a)
    override def show: String = typeName
  }
  def read[A](typeName: String)(f: String => Result[A]): Read[A] = Read[A](typeName)(f)
  def readWhen[A](r: Read[A])(p: A => Boolean): Read[A] = Read[A](r.show){ a =>
    r(a).ensure(NonEmptyList(new Throwable("Unmet type constraint")))(p)
  }
  def unsafeCoerce[A](s: String)(f: String => A): Result[A] = attempt(f(s))
}

/** A typeclass for safely converting a `String` to a concretely typed value.
  *
  * @tparam A value to convert to
  */
trait Read[A] {
  def apply(a: String): Result[A]
  def show: String
}
