package core

import core.Formatter.Lexical
import core.Read.Result

import scala.collection.generic.CanBuildFrom
import scalaz.syntax.validation._
import scalaz.syntax.applicative._

object Implicits {
  def unsafeCoerce[A](s: String)(f: String => A): Result[A] =
    try { f(s).successNel } catch { case e: Exception => e.failureNel }

  def association[A](prefix: Sym)(implicit proof: Read[A]): Read[A] = Read { s =>
    prefix
      .find(value => s startsWith value)
      .map(label => s drop label.value.length)
      .map(proof.apply)
      .getOrElse(new Throwable(s"Could not prove that the value of ${prefix.show} is of the desired type").failureNel)
  }

  implicit def stringToSym(s: String): Sym = Sym(s)
  implicit val intsReified: Read[Int] = Read[Int](unsafeCoerce(_)(_.toInt))
  implicit val booleansReified: Read[Boolean] = Read[Boolean](unsafeCoerce(_)(_.toBoolean))
  implicit val stringsReified: Read[String] = Read[String](_.successNel)
  implicit def traversablesReified[T[X] <: TraversableOnce[X], A](implicit proof: Read[A], cbf: CanBuildFrom[T[A], A, T[A]]): Read[T[A]] = Read { s =>
    s.split(",")
      .foldLeft(cbf().successNel[Throwable]) { (builder, value) =>
        (builder |@| proof(value))(_ += _)
      }
      .map(_.result())
  }

  implicit val charLexical: Lexical[Char] = new Lexical[Char] {

    override def blank = ' '

    override def break = '\n'

    override def continuation = '-'

    override def eq(a1: Char, a2: Char) = a1 == a2
  }
}
