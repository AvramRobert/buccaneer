import core.Implicits._
import core.{Denot, Label, Sym}

import scala.util.Random

class SymDenotSpec extends DefaultTestSuite {

  "Symbols" should {

    "support alternatives" in {
      forAll { (s: String, list: List[String]) =>
        val item = (s :: list)(Random.nextInt(list.size + 1))
        list.foldLeft(Label(s): Sym)(_ | _).find(_ == item).isDefined shouldBe true
      }
    }

    "support accurate string representations" in {
      forAll { (s: String, list: List[String]) =>
        val item = s :: list
        list.foldLeft(Label(s): Sym)(_ | _).show shouldBe item.mkString(" | ")
      }
    }
  }

  "Denotations" should {

    "support endomorphisms for their Docs" in {
      forAll { (id: String, text: String) =>
        val sym = Label(id)
        Denot.id(sym).mapDocs(_.mapMsg(_ => text)).docs.msg == text
        Denot.typing[Int](intsReified).mapDocs(_.mapMsg(_ => text)).docs.msg == text
        Denot.typedId[Int](sym, intsReified).mapDocs(_.mapMsg(_ => text)).docs.msg == text
      }
    }
  }
}