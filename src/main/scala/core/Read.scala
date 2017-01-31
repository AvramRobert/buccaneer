package core

import core.Read.Result
import scalaz.ValidationNel

object Read {
  type Result[A] = ValidationNel[Throwable, A]

  def apply[A](f: String => Result[A]): Read[A] = new Read[A] {
    override def apply(a: String): Result[A] = f(a)
  }
}

trait Read[A] {
  def apply(a: String): Result[A]
}
