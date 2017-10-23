package buccaneer

import java.io.File
import buccaneer.Read._
import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions
import scalaz.syntax.applicative._

trait ReadImplicits {
  implicit val readUnit: Read[Unit] = Read("Nothing") { input =>
    if (input.isEmpty) success(())
    else failure("")
  }
  implicit val readInt: Read[Int] = Read.attempt("Int")(_.toInt)
  implicit val readBool: Read[Boolean] = Read.attempt("Boolean")(_.toBoolean)
  implicit val readString: Read[String] = Read.attempt("String")(identity)
  implicit val readLong: Read[Long] = Read.attempt("Long")(_.toLong)
  implicit val readDouble: Read[Double] = Read("Double") { s =>
    if (s.contains(".")) unsafeCoerce(s)(_.toDouble)
    else failure(s"Cannot read double value of '$s'")
  }
  implicit val readFloat: Read[Float] = Read("Float") { s =>
    if (s.toLowerCase.endsWith("f")) unsafeCoerce(s)(_.toFloat)
    else failure(s"Cannot read float value of '$s'")
  }

  // I should probably add some syntactic characteristics to BigInts in order to avoid ambiguity
  implicit val readBigInt: Read[BigInt] = Read.attempt("BigInt")(x => BigInt(x))
  implicit val readBigDecimal: Read[BigDecimal] = Read("BigDecimal") { s =>
    if (s.toLowerCase.endsWith("d")) unsafeCoerce(s.dropRight(1))(x => BigDecimal(x.toString))
    else failure(s"Cannot read big decimal value of '$s'")
  }

  implicit val readFile: Read[File] = Read.attempt("Path")(x => new File(x))

  implicit def readColl[T[X] <: TraversableOnce[X], A](implicit proof: Read[A], cbf: CanBuildFrom[T[A], A, T[A]]): Read[T[A]] =
    Read(s"Coll[${proof.show}]") { s =>
      if (s.isEmpty) failure("Cannot read values from empty collection")
      else
        s.split(",").foldLeft(success(cbf())) { (builder, value) =>
        for {
          bld <- builder
          a <- proof(value)
        } yield bld += a
      }.map(_.result())
    }

  implicit def readMap[K, V](implicit proofK: Read[K], proofV: Read[V]): Read[Map[K, V]] =
    Read(s"Map[${proofK.show}, ${proofV.show}]") { s =>
      if (s.isEmpty) failure("Cannot read values from empty map")
      else {
        val empty = success(Map.empty[K, V])
        s.split(",").foldLeft(empty) { (vm, entry) =>
            entry.split("=").toList match {
              case key :: value :: Nil => for {
                  map <- vm
                  k <- proofK(key)
                  v <- proofV(value)
                } yield map + (k -> v)
              case _ => failure(s"Cannot read map entry of `$entry`")
            }
          }
      }
    }
}

object Implicits extends ReadImplicits
