import buccaneer.RoseList._
import buccaneer.{Bundle, Petal, RoseList, Stem}

class RoseListSpec extends DefaultTestSuite  {

  "A RoseList" should {

    "prepend petals" in {
      forAll { (a: Int, b: Int, c: Int) =>
        (RoseList.empty prepend a prepend b prepend c) shouldBe Petal(c, Petal(b, Petal(a, Stem)))
      }
    }

    "prepend bundles" in {
      forAll { (a: Int, b: List[Int]) =>
        whenever(b.size > 1) {
          (RoseList.empty prepend a prepend(b:_*)) shouldBe Bundle(b, Petal(a, Stem))
        }
      }
    }

    "expand exhaustively" in {
      forAll { (a: Int, b: Int, c: Int, d: Int) =>
        val variations = List(b, c, d)
        (RoseList.empty prepend a prepend(variations:_*)).expand shouldBe variations.permutations.toSet.map((bx: List[Int]) => a :: bx)
      }
    }
  }
}
