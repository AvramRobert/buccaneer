import java.io.File
import buccaneer.Implicits._
import buccaneer.Read.{readWhen, constrain}
import buccaneer.success

class ReadSpec extends DefaultTestSuite {

  "A Read typeclass" should {

    "successfully constrain reads based on predicates" in {
      forAll{ (i: Int) =>
        val max = 100

        whenever(i <= max) {
          readWhen(readInt)(_ > max)(i.toString).isLeft shouldBe true
        }
      }
    }

    "successfully allow reads when constraints are met" in {
      forAll { (i: Int) =>
        val max = 100

        whenever(i > max) {
          readWhen(readInt)(_ > max)(i.toString) shouldBe success(i)
        }
      }
    }

    "constrain inputs before reading" in {
      forAll { (i: Int) =>
        constrain(readInt)(_.split(":")(1))(s"a:$i") shouldBe success(i)
      }
    }

    "successfully read correct string-encoded values" in {
      forAll { (i: Int) =>
        readInt(i.toString) shouldBe success(i)
      }
    }

    "fail gracefully when passed wrong string-encoded values" in {
      forAll { (s: String) =>
        readInt(s).isLeft shouldBe true
      }
    }

    "read doubles" in {
      forAll { (d: Double) =>
        readDouble(d.toString) shouldBe success(d)
      }
    }

    "read floats" in {
      forAll { (f: Float) =>
        readFloat(f.toString + "f") shouldBe success(f)
      }
    }

    "read big ints" in {
      forAll { (b: BigInt) =>
        readBigInt(b.toString) shouldBe success(b)
      }
    }

    "read big decimals" in {
      forAll { (d: BigDecimal) =>
        readBigDecimal(s"${d}d") shouldBe success(d)
      }
    }

    "read booleans" in {
      forAll { (b: Boolean) =>
        readBool(b.toString) shouldBe success(b)
      }
    }

    "read arbitrary collections" in {
      forAll { (list: List[Int], vector: Vector[Double], set: Set[Boolean]) =>
        whenever(list.nonEmpty) {
          readColl[List, Int].apply(list.mkString(",")) shouldBe success(list)
        }
        whenever(vector.nonEmpty) {
          readColl[Vector, Double].apply(vector.mkString(",")) shouldBe success(vector)
        }
        whenever(set.nonEmpty) {
          readColl[Set, Boolean].apply(set.mkString(",")) shouldBe success(set)
        }
      }
    }

    "read maps" in {
      forAll { (map: Map[Int, Double]) =>
        whenever(map.nonEmpty) {
          readMap[Int, Double].apply(map.foldLeft("")((str, entry) => str + s"${entry._1}=${entry._2},").dropRight(1)) shouldBe success(map)
        }
      }
    }

    "read strings" in {
      forAll { (s: String) =>
        readString(s) shouldBe success(s)
      }
    }

    "read files" in {
      forAll { (r: String, d: String, h: String) =>
        val path = s"/$r/$d/$h"
        readFile(path) shouldBe success(new File(path))
      }
    }
  }
}

