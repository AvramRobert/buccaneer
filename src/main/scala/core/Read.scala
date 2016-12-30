package core

import core.Read.Result

import scalaz.{Bind, ValidationNel}

object Read {
  type Result[A] = ValidationNel[Throwable, A]

  implicit val bindResult: Bind[Result] = new Bind[Result] {
    override def bind[A, B](fa: Result[A])(f: (A) => Result[B]): Result[B] = fa flatMap f

    override def map[A, B](fa: Result[A])(f: (A) => B): Result[B] = fa map f
  }

  def apply[A](f: String => Result[A]): Read[A] = new Read[A] {
    override def apply(a: String): Result[A] = f(a)
  }
}

trait Read[A] {
  def apply(a: String): Result[A]
}
