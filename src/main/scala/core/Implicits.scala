package core

import java.io.File

import core.Read._
import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions
import scalaz.syntax.applicative._

object Implicits {
  implicit def stringToSym(s: String): Sym = Label(s)

  implicit val readUnit: Read[Unit] = Read[Unit](_ => success(()))
  implicit val readInt: Read[Int] = Read[Int](unsafeCoerce(_)(_.toInt))
  implicit val readBool: Read[Boolean] = Read[Boolean](unsafeCoerce(_)(_.toBoolean))
  implicit val readString: Read[String] = Read[String](success)
  implicit val readDouble: Read[Double] = Read[Double] { s =>
    if (s.contains(".")) unsafeCoerce(s)(_.toDouble)
    else failure(new Throwable(s"Cannot read double value of '$s'"))
  }
  implicit val readFloat: Read[Float] = Read[Float] { s =>
    if (s.toLowerCase.endsWith("f")) unsafeCoerce(s)(_.toFloat)
    else failure(new Throwable(s"Cannot read float value of '$s'"))
  }

  // I should probably add some syntactic characteristics to BigInts in order to avoid ambiguity
  implicit val readBigInt: Read[BigInt] = Read[BigInt](unsafeCoerce(_)(x => BigInt(x)))
  implicit val readBigDecimal: Read[BigDecimal] = Read[BigDecimal] { s =>
    if(s.toLowerCase.endsWith("d")) unsafeCoerce(s.dropRight(1))(x => BigDecimal(x.toString))
    else failure(new Throwable(s"Cannot read big decimal value of '$s'"))
  }

  implicit val readFile: Read[File] = Read[File](unsafeCoerce(_)(x => new File(x)))

  implicit def readColl[T[X] <: TraversableOnce[X], A](implicit proof: Read[A], cbf: CanBuildFrom[T[A], A, T[A]]): Read[T[A]] = Read { s =>
    if (s.isEmpty) success(cbf().result())
    else s.split(",")
      .foldLeft(success(cbf())) { (builder, value) =>
        (builder |@| proof(value)) (_ += _)
      }
      .map(_.result())
  }

  implicit def readMap[K, V](implicit proofK: Read[K], proofV: Read[V]): Read[Map[K, V]] = Read { s =>
    val empty = success(Map.empty[K, V])
    if (s.isEmpty) empty
    else s.split(",")
      .foldLeft(empty) { (vm, entry) =>
        entry.split("=").toList match {
          case key :: value :: Nil =>
            (vm |@| proofK(key) |@| proofV(value)) ((m, k, v) => m + (k -> v))
          case _ => failure(new Throwable(s"Cannot read map entry of `$entry`"))
        }
      }
  }
}
