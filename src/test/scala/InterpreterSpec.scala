import buccaneer.Implicits._
import buccaneer.everything._
import buccaneer.{Step, Validators}

class InterpreterSpec extends DefaultTestSuite {

  "An interpreter" should {

    val msg = "Shouldnt've gotten here"

    def checkSucc[A <: Any, B](s: Step[A], expected: B) = s.fold(_ shouldBe expected)(_ => fail(msg))(_ => fail(msg))

    def checkFail[A <: Any](s: Step[A])(p: List[Throwable] => Boolean) = s.fold(_ => fail(msg))(l => p(l) shouldBe true)(_ => fail(msg))

    def checkMeta[A <: Any](s: Step[A])(p: String => Boolean) = s.fold(_ => fail(msg))(_ => fail(msg))(s => p(s) shouldBe true)

    def aBoolean(labels: String*) = assignment[Boolean](labels: _*)("=")
    def aInt(labels: String*) = assignment[Int](labels: _*)("=")
    val argInt = argument[Int]
    val argNothing = argument[Unit]
    val optA = option("-a")
    val optB = option("-b")

    "correctly interpret single commands" in {
      forAll { (name: String, a: Int, b: Int, f: ((Int, Int)) => Int) =>
        val cmd = (>> - command(name) - argInt - argInt).apply(f)
        checkSucc(Interpreter.interpret(cmd).run(List(name, a.toString, b.toString)), f(a, b))
      }
    }

    "correctly interpret commands without arguments" in {
      forAll { (name: String, a: Int, b: Int, f: ((Int, Int)) => Int) =>
        val cli = Cli(
          (>> - command(name) - argInt - argInt).apply(f),
          (>> - argNothing).apply { _ => f(a, b) }
        )

        checkSucc(Interpreter.interpret(cli).run(List(name, a.toString, b.toString)), f(a, b))
        checkSucc(Interpreter.interpret(cli).run(List()), f(a, b))
      }
    }

    "correctly pick-out commands from a cli and interpret them" in {
      forAll { (name: String, int: Int, bool: Boolean, string: String, f: ((Int, Int)) => Int, g: Boolean => Boolean) =>
        val h = (arg: (Boolean, Int, Int)) => g(arg._1) && f(arg._2, arg._3) > 0
        val cmd1 = (>> - command(name) - argInt - argInt).apply(f)
        val cmd2 = (>> - command(name) - aBoolean(string)).apply(g)
        val cmd3 = (>> - command(name) - aBoolean(string) - argInt - argInt).apply (h)
        val cmd4 = (>> - command(string) - argInt - argInt).apply(f)
        val cmd5 = (>> - command(string) - aInt(string)).apply (identity)
        val cmd6 = (>> - command(string) - (optA, optB)).apply (identity)
        val cli = Cli(cmd1, cmd2, cmd3, cmd4, cmd5, cmd6)
        val interpreter = Interpreter.interpret(cli)
        checkSucc(interpreter.run(List(name, int.toString, int.toString)), f((int, int)))
        checkSucc(interpreter.run(List(name, s"$string=$bool")), g(bool))
        checkSucc(interpreter.run(List(name, s"$string=$bool", int.toString, int.toString)), h((bool, int, int)))
        checkSucc(interpreter.run(List(string, int.toString, int.toString)), f((int, int)))
        checkSucc(interpreter.run(List(string, s"$string=$int")), int)
        checkSucc(interpreter.run(List(string, "-a", "-b")), ())
        checkSucc(interpreter.run(List(string, "-b", "-a")), ())
      }
    }

    "fail gracefully when no matching commands can be found" in {
      forAll { (name: String, int: Int, bool: Boolean, string: String, f: ((Int, Int)) => Int, g: Boolean => Boolean) =>
        whenever(name != string) {
          val h = (arg: (Boolean, Int, Int)) => g(arg._1) && f(arg._2, arg._3) > 0
          val cmd1 = (>> - command(name) - argInt - argInt).apply(f)
          val cmd2 = (>> - command(name) - aBoolean(string)).apply(g)
          val cmd3 = (>> - command(name) - aBoolean(string) - argInt - argInt).apply (h)
          val cmd4 = (>> - command(string) - argInt - argInt).apply(f)
          val cmd5 = (>> - command(string) - aInt(string)).apply (identity)
          val cli = Cli(cmd1, cmd2, cmd3, cmd4, cmd5)
          val interpreter = Interpreter.interpret(cli)

          checkFail(interpreter.run(List("nocmd", int.toString, int.toString)))(_.nonEmpty)
          checkFail(interpreter.run(List(name, int.toString, "bla")))(_.nonEmpty)
          checkFail(interpreter.run(List(name, s"$string=$int")))(_.nonEmpty)
          checkFail(Interpreter.interpret(cmd1).run(List(name, "a", "b")))(_.nonEmpty)
          checkFail(Interpreter.interpret(cmd3).run(List(name, s"$string=$int", int.toString, "x")))(_.nonEmpty)
        }
      }
    }

    "support suggestions and help at any point during the invocation" in {
      forAll { (name: String, int: Int, bool: Boolean, string: String, f: ((Int, Int)) => Int, g: Boolean => Boolean) =>
        whenever(name != string) {
          val h = (arg: (Boolean, Int, Int)) => g(arg._1) && f(arg._2, arg._3) > 0
          val cmd1 = (>> - command(name) - argInt - argInt).apply(f)
          val cmd2 = (>> - command(name) - aBoolean(string)).apply(g)
          val cmd3 = (>> - command(name) - aBoolean(string) - argInt - argInt).apply (h)
          val cmd4 = (>> - command(string) - argInt - argInt).apply(f)
          val cmd5 = (>> - command(string) - aInt(string)).apply (identity)
          val cli = Cli(cmd1, cmd2, cmd3, cmd4, cmd5)
          val interpreter = Interpreter.interpretH(cli)

          checkMeta(interpreter.run(List(name, "--help")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, "--suggest")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, int.toString, "--help")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, int.toString, "--suggest")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, int.toString, int.toString, "--help")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, int.toString, int.toString, "--suggest")))(_.nonEmpty)
        }
      }
    }

    "match commands partially" in {
      forAll { (name: String, int: Int, bool: Boolean, string: String, f: ((Int, Int)) => Int, g: Boolean => Boolean) =>
        whenever((name != string) && name.trim.nonEmpty && string.trim.nonEmpty) {
          val h = (arg: (Boolean, Int, Int)) => g(arg._1) && f(arg._2, arg._3) > 0
          val cmd1 = (>> - command(name) - argInt - argInt).apply(f)
          val cmd2 = (>> - command(name) - aBoolean(string)).apply(g)
          val cmd3 = (>> - command(name) - aBoolean(string) - argInt - argInt).apply (h)
          val cmd4 = (>> - command(string) - argInt - argInt).apply(f)
          val cmd5 = (>> - command(string) - aInt(string)).apply (identity)
          val cli = Cli(cmd1, cmd2, cmd3, cmd4, cmd5)
          val interpreter = Interpreter.interpret(cli)

          Interpreter.partialMatch(cli.keySet, List(name)) shouldBe cmd1.expr.expand ++ cmd2.expr.expand ++ cmd3.expr.expand
          Interpreter.partialMatch(cli.keySet, List(name, int.toString)) shouldBe cmd1.expr.expand
          Interpreter.partialMatch(cli.keySet, List(name, s"$string$bool")) shouldBe cmd2.expr.expand ++ cmd3.expr.expand
        }
      }
    }

    "validate denotations" in {
      forAll { (id: String, value: Int) =>
        whenever(id.trim.nonEmpty) {
          Validators.syntax((command(id), Some(id))).isSuccess shouldBe true
          Validators.syntax((aInt(id), Some(s"$id=$value"))).isSuccess shouldBe true
          Validators.syntax((argInt, Some(value.toString))).isSuccess shouldBe true
          Validators.types((argInt, Some(value.toString))).isSuccess shouldBe true
          Validators.syntax((aInt(id), Some(s"otherId=$value"))).isFailure shouldBe true
          Validators.types((aBoolean(id), Some(s"$id=$value"))).isFailure shouldBe true
          Validators.syntax((command(id), None)).isFailure shouldBe true
          Validators.types((argInt, None)).isFailure shouldBe true
        }
      }
    }
  }
}
