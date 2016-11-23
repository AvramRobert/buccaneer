package core

import core.Reified.Result

import scala.collection.generic.CanBuildFrom
import scalaz.syntax.validation._
import scalaz.syntax.applicative._

object Implicits {
  def unsafeCoerce[A](s: String)(f: String => A): Result[A] =
    try { f(s).successNel } catch { case e: Exception => e.failureNel }

  def association[A](prefix: Sym)(implicit proof: Reified[A]): Reified[A] = Reified { s =>
    prefix
      .find(value => s startsWith value)
      .map(label => s drop label.value.length)
      .map(proof.apply)
      .getOrElse(new Throwable(s"Could not prove that the value of ${prefix.show} is of the desired type").failureNel)
  }

  implicit def stringToSym(s: String): Sym = Label(s)
  implicit val intsReified: Reified[Int] = Reified[Int](unsafeCoerce(_)(_.toInt))
  implicit val booleansReified: Reified[Boolean] = Reified[Boolean](unsafeCoerce(_)(_.toBoolean))
  implicit val stringsReified: Reified[String] = Reified[String](_.successNel)
  implicit def traversablesReified[T[X] <: TraversableOnce[X], A](implicit proof: Reified[A], cbf: CanBuildFrom[T[A], A, T[A]]): Reified[T[A]] = Reified { s =>
    s.split(",")
      .foldLeft(cbf().successNel[Throwable]) { (builder, value) =>
        (builder |@| proof(value))(_ += _)
      }
      .map(_.result())
  }
}
