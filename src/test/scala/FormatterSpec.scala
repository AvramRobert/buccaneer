import core.{Formatter, HelpConfig}

class FormatterSpec extends DefaultTestSuite {

  "A Formatter" should {

    val config = HelpConfig(10, 0, 1)
    //    "have an identity" in {
    //      forAll { (v: Vector[Char]) =>
    //        (Formatter(v) absorb Formatter.empty).evaluate._1 shouldBe v
    //      }
    //    }

    "morph through continuation" in {
      forAll { (v: Vector[Char]) =>
        Formatter(v).continue(_.reverse).evaluate._1 shouldBe v.reverse
      }
    }

    "support simple appensions and prepensions" in {
      forAll { (c: Char, v: Vector[Char]) =>
        whenever(v.nonEmpty) {
          Formatter(v).append(c).evaluate._1 shouldBe v :+ c
          Formatter(v).prepend(c).evaluate._1 shouldBe c +: v
        }
      }
    }

    "apply formatting to one line" in {
      forAll { (v: Vector[Char]) =>
        whenever(v.nonEmpty) {
          val expected = (v.take(config.textWidth) :+ '\n') ++ v.drop(config.textWidth)
          Formatter(v).ofWidth(config.textWidth).withLines.evaluate._1 shouldBe expected
        }
      }
    }

    "apply formatting to every line" in {
      forAll { (s: String) =>
        whenever(s.nonEmpty) {
          val v = s.toCharArray.toVector
          Formatter(v).ofWidth(config.textWidth).run shouldBe v.grouped(config.textWidth).flatMap(_ :+ '\n').toVector
        }
      }
    }

    "support padding" in {
      forAll { (v: Vector[Char]) =>
        Formatter(v).
          ofWidth(config.textWidth).
          fillAll.
          run shouldBe v.grouped(config.textWidth).flatMap { v =>
            val x = (0 until (config.textWidth - v.size)).map(_ => ' ').toVector
            (v ++ x) :+ '\n'
        }.toVector
      }
    }
  }
}
