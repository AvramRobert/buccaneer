package core
import Implicits._
/*
 * TODO: Are type safe partial HMaps possible?
 * Apparently shapeless Records might be able to do what I want with partial HMaps
 * val records = addNamed.syntax ->> addNamed :: addAssigned.syntax ->> addAssigned :: shift.syntax ->> shift :: HNil
 * records.get(addNamed.syntax) => should be a Runner[A]
 * Problems: implicits
 */

object Main {
  def main(args: Array[String]) = {

    def command(label: String) = Command(label)
    val add = command("add")

    val addUnnamed =
      add
        .unnamed[Int]
        .unnamed[Int]
        .apply(_ + _)

    val addNamed =
      add
        .named[Int]("a")
        .named[Int]("b")
        .apply(_ + _)

    val addAssigned =
      add
        .assignment[Int]("a", ":")
        .assignment[Int]("b", ":")
        .apply(_ + _)

    val shift =
      command("shift")
        .unnamed[String]
        .unnamed[Int]
        .apply { (a, b) =>
          a + b
        }


    val params1 = List("add", "2", "3")
    val params2 = List("add", "a", "2", "b", "3")
    val params3 = List("add", "a:2", "b:3")
    val params4 = List("shift", "a", "1")
    val params5 = List("subtract", "5", "4")

    val cli =
      Cli.
        command("add") {
          _.
            named[Int]("a").
            named[Int]("b").
            apply(_ + _)
        }.
        child {
          _.
            unnamed[Int].
            unnamed[Int].
            apply(_ + _)
        }.
        child {
          _.
            assignment[Int]("a", ":").
            assignment[Int]("b", ":").
            apply(_ + _)
        }.

        command("subtract") {
          _.
            named[Int]("a").
            named[Int]("b").
            apply(_ - _)
        }.
        child {
          _.
            unnamed[Int].
            unnamed[Int].
            apply(_ - _)
        }.
        commandT("check") {
          _.
            unnamed[String].
            apply(_ == "-r")
        }
    println(Interpreter.interpret(cli).run(params5))
  }

}
