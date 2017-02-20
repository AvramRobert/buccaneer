import core.{Formatter, ManConfig}

class FormatterSpec extends DefaultTestSuite {

  "A Formatter" should {

    val config = ManConfig(10, 0, 1)
    //    "have an identity" in {
    //      forAll { (v: Vector[Char]) =>
    //        (Formatter(v) absorb Formatter.empty).evaluate._1 shouldBe v
    //      }
    //    }

    "morph through continuation" in {
      forAll { (v: Vector[Char]) =>
        Formatter(v).continue(_.reverse).evaluate shouldBe v.reverse
      }
    }

    "support simple appensions and prepensions" in {
      forAll { (c: Char, v: Vector[Char]) =>
        whenever(v.nonEmpty) {
          Formatter(v).append(c).evaluate shouldBe v :+ c
          Formatter(v).prepend(c).evaluate shouldBe c +: v
        }
      }
    }

    "apply formatting to one line" in {
      forAll { (v: Vector[Char]) =>
        whenever(v.nonEmpty) {
          val expected = (v.take(config.textWidth) :+ '\n') ++ v.drop(config.textWidth)
          Formatter(v).ofWidth(config.textWidth).withLines.evaluate shouldBe expected
        }
      }
    }

    "apply formatting to every line" in {
      forAll { (v: Vector[Char]) =>
        whenever(v.nonEmpty) {
          Formatter(v).ofWidth(config.textWidth).run shouldBe v.grouped(config.textWidth).flatMap(_ :+ '\n').toVector
        }
      }
    }

    "support padding" in {
      forAll { (v: Vector[Char]) =>
        Formatter(v).
          ofWidth(config.textWidth).
          fill.
          run shouldBe v.grouped(config.textWidth).flatMap { v =>
          val x = (0 until (config.textWidth - v.size)).map(_ => ' ').toVector
          (v ++ x) :+ '\n'
        }.toVector
      }
    }

    "be able to equate its size to that of another formatter" in {
      forAll { (v1: Vector[Char], v2: Vector[Char]) =>
        whenever(v1.nonEmpty && v2.nonEmpty) {
          val f1 = Formatter(v1).ofWidth(5)
          val f2 = Formatter(v2).ofWidth(5)
          val (x1, x2) = f1.equate(f2)
          x1.totalLines shouldBe x2.totalLines
        }
      }
    }

    "interleave two formattings" in {
      forAll { (v1: Vector[Char], v2: Vector[Char]) =>
        whenever((v1.size > config.textWidth) && (v2.size > config.textWidth)) {
          val f1 = Formatter(v1).ofWidth(config.textWidth).fill
          val f2 = Formatter(v2).ofWidth(config.textWidth).fill
          f1.evaluate.
            containsSlice {
            f1.interleave(f2).
              evaluate.
              grouped(config.textWidth).
              zipWithIndex.
              filter(_._2 % 2 == 0).
              flatMap(_._1).
              toVector
          } shouldBe true
        }
      }
    }
  }
}
