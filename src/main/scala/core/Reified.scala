package core

import scalaz.\/

object Reified {
  def apply[A](f: String => Throwable \/ A): Reified[A] = new Reified[A] {
    override def apply(a: String): Throwable \/ A = f(a)
  }
}

trait Reified[A] {
  def apply(a: String): Throwable \/ A
}
