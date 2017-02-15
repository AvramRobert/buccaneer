import core.Store
import core.Store._

class StoreSpec extends DefaultTestSuite {

  "A store" should {

    "behave like any other Map" in {
      forAll { (keyValue: Map[Int, String]) =>

      }
    }

    "be typesafe in case of homogeneous types" in {

    }

    "widen its type to `Any` in case of heterogeneity" in {

    }
  }
}

// Test store type classes
// Test store type widening
