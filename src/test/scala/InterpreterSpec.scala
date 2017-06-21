import buccaneer.Implicits._
import buccaneer.everything._
import buccaneer.{Step, Validators, Denot, Label}

class InterpreterSpec extends DefaultTestSuite {

  "An interpreter" should {

    val msg = "Shouldnt've gotten here"

    def checkSucc[A <: Any, B](s: Step[A], expected: B) = s.fold(_ shouldBe expected)(_ => fail(msg))(_ => fail(msg))

    def checkFail[A <: Any](s: Step[A])(p: List[Throwable] => Boolean) = s.fold(_ => fail(msg))(l => p(l) shouldBe true)(_ => fail(msg))

    def checkMeta[A <: Any](s: Step[A])(p: String => Boolean) = s.fold(_ => fail(msg))(_ => fail(msg))(s => p(s) shouldBe true)

    "correctly interpret single commands" in {
      forAll { (name: String, a: Int, b: Int, f: (Int, Int) => Int) =>
        val cmd = (command(name) - argument[Int] - argument[Int]) (f)
        checkSucc(Interpreter.interpret(cmd).run(List(name, a.toString, b.toString)), f(a, b))
      }
    }

    "correctly interpret commands without arguments" in {
      forAll { (name: String, a: Int, b: Int, f: (Int, Int) => Int) =>
        val empty = argument[Unit]
        val cli = Cli(
          (command(name) - argument[Int] - argument[Int])(f),
          empty { _ => f(a, b) }
        )

        checkSucc(Interpreter.interpretH(cli).run(List(name, a.toString, b.toString)), f(a, b))
        checkSucc(Interpreter.interpretH(cli).run(List()), f(a, b))
      }
    }

    "correctly pick-out commands from a cli and interpret them" in {
      forAll { (name: String, int: Int, bool: Boolean, string: String, f: (Int, Int) => Int, g: Boolean => Boolean) =>
        val h = (x: Boolean, y: Int, z: Int) => g(x) && f(y, z) > 0
        val cmd1 = (command(name) - argument[Int] - argument[Int]) (f)
        val cmd2 = (command(name) - assignment[Boolean](string)) (g)
        val cmd3 = (command(name) - assignment[Boolean](string) - argument[Int] - argument[Int]) (h)
        val cmd4 = (command(string) - argument[Int] - argument[Int]) (f)
        val cmd5 = (command(string) - assignment[Int](name)) (identity)
        val cli = Cli(cmd1, cmd2, cmd3, cmd4, cmd5)
        val interpreter = Interpreter.interpret(cli)
        checkSucc(interpreter.run(List(name, int.toString, int.toString)), f(int, int))
        checkSucc(interpreter.run(List(name, s"$string$bool")), g(bool))
        checkSucc(interpreter.run(List(name, s"$string$bool", int.toString, int.toString)), h(bool, int, int))
        checkSucc(interpreter.run(List(string, int.toString, int.toString)), f(int, int))
        checkSucc(interpreter.run(List(string, s"$name$int")), int)
      }
    }

    "fail gracefully when no matching commands can be found" in {
      forAll { (name: String, int: Int, bool: Boolean, string: String, f: (Int, Int) => Int, g: Boolean => Boolean) =>
        whenever(name != string) {
          val h = (x: Boolean, y: Int, z: Int) => g(x) && f(y, z) > 0
          val cmd1 = (command(name) - argument[Int] - argument[Int]) (f)
          val cmd2 = (command(name) - assignment[Boolean](string)) (g)
          val cmd3 = (command(name) - assignment[Boolean](string) - argument[Int] - argument[Int]) (h)
          val cmd4 = (command(string) - argument[Int] - argument[Int]) (f)
          val cmd5 = (command(string) - assignment[Int](name)) (identity)
          val cli = Cli(cmd1, cmd2, cmd3, cmd4, cmd5)
          val interpreter = Interpreter.interpret(cli)

          checkFail(interpreter.run(List("nocmd", int.toString, int.toString)))(_.nonEmpty)
          checkFail(interpreter.run(List(name, int.toString, "bla")))(_.nonEmpty)
          checkFail(interpreter.run(List(name, s"$string$int")))(_.nonEmpty)
          checkFail(Interpreter.interpret(cmd1).run(List(name, "a", "b")))(x => x.nonEmpty && x.size == 2)
          checkFail(Interpreter.interpret(cmd3).run(List(name, s"$string$int", int.toString, "x")))(x => x.nonEmpty && x.size == 2)
        }
      }
    }

    "support suggestions and help at any point during the invocation" in {
      forAll { (name: String, int: Int, bool: Boolean, string: String, f: (Int, Int) => Int, g: Boolean => Boolean) =>
        whenever(name != string) {
          val h = (x: Boolean, y: Int, z: Int) => g(x) && f(y, z) > 0
          val cmd1 = (command(name) - argument[Int] - argument[Int]) (f)
          val cmd2 = (command(name) - assignment[Boolean](string)) (g)
          val cmd3 = (command(name) - assignment[Boolean](string) - argument[Int] - argument[Int]) (h)
          val cmd4 = (command(string) - argument[Int] - argument[Int]) (f)
          val cmd5 = (command(string) - assignment[Int](name)) (identity)
          val cli = Cli(cmd1, cmd2, cmd3, cmd4, cmd5)
          val interpreter = Interpreter.interpretH(cli)

          checkMeta(interpreter.run(List(name, "--help")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, "--sgst")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, int.toString, "--help")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, int.toString, "--sgst")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, int.toString, int.toString, "--help")))(_.nonEmpty)
          checkMeta(interpreter.run(List(name, int.toString, int.toString, "--sgst")))(_.nonEmpty)
        }
      }
    }


    "match commands partially" in {
      forAll { (name: String, int: Int, bool: Boolean, string: String, f: (Int, Int) => Int, g: Boolean => Boolean) =>
        whenever((name != string) && name.trim.nonEmpty && string.trim.nonEmpty) {
          val h = (x: Boolean, y: Int, z: Int) => g(x) && f(y, z) > 0
          val cmd1 = (command(name) - argument[Int] - argument[Int]) (f)
          val cmd2 = (command(name) - assignment[Boolean](string)) (g)
          val cmd3 = (command(name) - assignment[Boolean](string) - argument[Int] - argument[Int]) (h)
          val cmd4 = (command(string) - argument[Int] - argument[Int]) (f)
          val cmd5 = (command(string) - assignment[Int](name)) (identity)
          val cli = Cli(cmd1, cmd2, cmd3, cmd4, cmd5)

          Interpreter.partialMatch(cli.keySet, List(name)) shouldBe Set(cmd1.syntax, cmd2.syntax, cmd3.syntax)
          Interpreter.partialMatch(cli.keySet, List(name, int.toString)) shouldBe Set(cmd1.syntax)
          Interpreter.partialMatch(cli.keySet, List(name, s"$string$bool")) shouldBe Set(cmd2.syntax, cmd3.syntax)
        }
      }
    }

    "validate denotations" in {
      forAll { (id: String, value: Int) =>
        whenever(id.trim.nonEmpty) {
          Validators.syntax((Denot.id(Label(id)), id)).isSuccess shouldBe true
          Validators.syntax((Denot.typedId(Label(id), readInt), s"$id$value")).isSuccess shouldBe true
          Validators.syntax((Denot.typing(readInt), value.toString)).isSuccess shouldBe true
          Validators.types((Denot.typing(readInt), value.toString)).isSuccess shouldBe true
          Validators.syntax((Denot.typedId(Label(id), readInt), s"noId$value")).isFailure shouldBe true
          Validators.types((Denot.typedId(Label(id), readBool), s"$id$value")).isFailure shouldBe true
        }
      }
    }
  }
}
