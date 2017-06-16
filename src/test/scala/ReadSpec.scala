import java.io.File

import core.Implicits._

import core.Read.readWhen
import scalaz.Success

class ReadSpec extends DefaultTestSuite {

  "A Read typeclass" should {

    "successfully constrain reads based on predicates" in {
      forAll{ (i: Int) =>
        val max = 100

        whenever(i <= max) {
          readWhen(readInt)(_ > max)(i.toString).isFailure shouldBe true
        }
      }
    }

    "successfully allow reads when constraints are met" in {
      forAll { (i: Int) =>
        val max = 100

        whenever(i > max) {
          readWhen(readInt)(_ > max)(i.toString) shouldBe Success(i)
        }
      }
    }

    "successfully read correct string-encoded values" in {
      forAll { (i: Int) =>
        readInt(i.toString) shouldBe Success(i)
      }
    }

    "fail gracefully when passed wrong string-encoded values" in {
      forAll { (s: String) =>
        readInt(s).isFailure shouldBe true
      }
    }

    "read doubles" in {
      forAll { (d: Double) =>
        readDouble(d.toString) shouldBe Success(d)
      }
    }

    "read floats" in {
      forAll { (f: Float) =>
        readFloat(f.toString + "f") shouldBe Success(f)
      }
    }

    "read big ints" in {
      forAll { (b: BigInt) =>
        readBigInt(b.toString) shouldBe Success(b)
      }
    }

    "read big decimals" in {
      forAll { (d: BigDecimal) =>
        readBigDecimal(s"${d}d") shouldBe Success(d)
      }
    }

    "read booleans" in {
      forAll { (b: Boolean) =>
        readBool(b.toString) shouldBe Success(b)
      }
    }

    "read arbitrary collections" in {
      forAll { (list: List[Int], vector: Vector[Double], set: Set[Boolean]) =>
        whenever(list.nonEmpty) {
          readColl[List, Int].apply(list.mkString(",")) shouldBe Success(list)
        }
        whenever(vector.nonEmpty) {
          readColl[Vector, Double].apply(vector.mkString(",")) shouldBe Success(vector)
        }
        whenever(set.nonEmpty) {
          readColl[Set, Boolean].apply(set.mkString(",")) shouldBe Success(set)
        }
      }
    }

    "read maps" in {
      forAll { (map: Map[Int, Double]) =>
        whenever(map.nonEmpty) {
          readMap[Int, Double].apply(map.foldLeft("")((str, entry) => str + s"${entry._1}=${entry._2},").dropRight(1)) shouldBe Success(map)
        }
      }
    }

    "read strings" in {
      forAll { (s: String) =>
        readString(s) shouldBe Success(s)
      }
    }

    "read files" in {
      forAll { (r: String, d: String, h: String) =>
        val path = s"/$r/$d/$h"
        readFile(path) shouldBe Success(new File(path))
      }
    }
  }
}

