package core

import core.Read.Result
import scalaz.ValidationNel
import scalaz.syntax.validation._

object Read {
  type Result[A] = ValidationNel[Throwable, A]

  def apply[A](f: String => Result[A]): Read[A] = new Read[A] {
    override def apply(a: String): Result[A] = f(a)
  }
  def success[A](a: A) = a.successNel[Throwable]
  def failure[A](t: Throwable) = t.failureNel[A]
}

/** A typeclass for safely converting a `String` to a concretely typed value.
  *
  * @tparam A value to convert to
  */
trait Read[A] {
  def apply(a: String): Result[A]
}
