package buccaneer

import scala.util.control.NonFatal
import scalaz.NonEmptyList
import scalaz.syntax.validation._

object Read {
  def apply[A](typeName: String)(f: String => Result[A]): Read[A] = new Read[A] {
    override def apply(a: String): Result[A] = f(a)
    override def show: String = typeName
  }
  def constrain[A](r: Read[A])(f: String => String): Read[A] = new Read[A] {
    override def apply(a: String): Result[A] = unsafeCoerce(a)(f).fold(x => x.failure, s => r(s))
    override def show: String = r.show
  }
  def attempt[A](typeName: String)(f: String => A): Read[A] = Read[A](typeName)(unsafeCoerce(_)(f))
  def readWhen[A](r: Read[A])(p: A => Boolean): Read[A] = Read[A](r.show){ a =>
      r(a).ensure(NonEmptyList(new Throwable("Unmet type constraint")))(p)
  }
  def unsafeCoerce[A](s: String)(f: String => A): Result[A] =
    try { success(f(s)) } catch { case NonFatal(t) => failure(t) }
}

/** A typeclass for safely converting a `String` to a concretely typed value.
  *
  * @tparam A value to convert to
  */
trait Read[A] {
  def apply(a: String): Result[A]
  def show: String
}
