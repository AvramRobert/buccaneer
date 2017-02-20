package core

import java.io.File
import core.Read.Result
import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions
import scalaz.syntax.validation._
import scalaz.syntax.applicative._

object Implicits {
  def unsafeCoerce[A](s: String)(f: String => A): Result[A] =
    try {
      f(s).successNel
    } catch {
      case e: Exception => e.failureNel
    }

  implicit def stringToSym(s: String): Sym = Label(s)

  implicit val readInt: Read[Int] = Read[Int](unsafeCoerce(_)(_.toInt))
  implicit val readBool: Read[Boolean] = Read[Boolean](unsafeCoerce(_)(_.toBoolean))
  implicit val readString: Read[String] = Read[String](_.successNel)
  implicit val readDouble: Read[Double] = Read[Double] { s =>
    if (s.contains(".")) unsafeCoerce(s)(_.toDouble)
    else new NumberFormatException(s"Cannot read double value of '$s'").failureNel
  }
  implicit val readFloat: Read[Float] = Read[Float] { s =>
    if (s.toLowerCase.contains("f")) unsafeCoerce(s)(_.toFloat)
    else new NumberFormatException(s"Cannot read float value of '$s'").failureNel
  }

  // I should probably add some syntactic characteristics to BigInts and BigDecimals in order to avoid ambiguity
  implicit val readBigInt: Read[BigInt] = Read[BigInt](unsafeCoerce(_)(x => BigInt(x)))
  implicit val readBigDecimal: Read[BigDecimal] = Read[BigDecimal](unsafeCoerce(_)(x => BigDecimal(x)))

  implicit val readFile: Read[File] = Read[File](unsafeCoerce(_)(x => new File(x)))

  implicit def readColl[T[X] <: TraversableOnce[X], A](implicit proof: Read[A], cbf: CanBuildFrom[T[A], A, T[A]]): Read[T[A]] = Read { s =>
    if (s.isEmpty) cbf().result().successNel
    else s.split(",")
      .foldLeft(cbf().successNel[Throwable]) { (builder, value) =>
        (builder |@| proof(value)) (_ += _)
      }
      .map(_.result())
  }

  implicit def readMap[K, V](implicit proofK: Read[K], proofV: Read[V]): Read[Map[K, V]] = Read { s =>
    if (s.isEmpty) Map.empty[K, V].successNel
    else s.split(",")
      .foldLeft(Map.empty[K, V].successNel[Throwable]) { (vm, entry) =>
        entry.split("=").toList match {
          case key :: value :: Nil =>
            (vm |@| proofK(key) |@| proofV(value)) ((m, k, v) => m + (k -> v))
          case _ => new Throwable(s"Cannot read map entry of `$entry`").failureNel
        }
      }
  }
}
