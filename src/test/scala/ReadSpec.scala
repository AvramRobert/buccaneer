import core.Implicits._
import scalaz.Success

class ReadSpec extends DefaultTestSuite {

  "A Read typeclass" should {

    "successfully parse correct string-encoded values" in {
      forAll { (i: Int) =>
        readInt(i.toString) shouldBe Success(i)
      }
    }

    "fail gracefully when passed wrong string-encoded values" in {
      forAll { (s: String) =>
        readInt(s).isFailure shouldBe true
      }
    }
  }
}

