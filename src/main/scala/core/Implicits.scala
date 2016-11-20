package core

import scala.collection.generic.CanBuildFrom
import scalaz.\/
import scalaz.syntax.either._

object Implicits {
  def unsafeCoerce[A](s: String)(f: String => A): Throwable \/ A =
    try { f(s).right } catch { case e: Exception => e.left }

  implicit val ints: Reified[Int] = Reified[Int](unsafeCoerce(_)(_.toInt))
  implicit val booleans: Reified[Boolean] = Reified[Boolean](unsafeCoerce(_)(_.toBoolean))
  implicit val strings: Reified[String] = Reified[String](_.right)

  implicit def traversables[T[X] <: TraversableOnce[X], A](implicit proof: Reified[A], cbf: CanBuildFrom[T[A], A, T[A]]): Reified[T[A]] = Reified { s =>
    s.split(",")
      .foldLeft(cbf().right[Throwable]) { (builder, value) =>
        for {
          bld <- builder
          v <- proof(value)
        } yield bld += v
      }
      .map(_.result())
  }
}
