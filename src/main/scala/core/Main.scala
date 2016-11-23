package core

import Implicits._
import Store._

object Main {
  def main(args: Array[String]) = {

    val add = Command("add")
    val shift = Command("shift")

    val addUnnamed =
      add
        .argument[Int]
        .argument[Int]
        .apply(_ + _)

    val addNamed =
      add
        .option("a").argument[Int]
        .option("b").argument[Int]
        .apply(_ + _)

    val addAssigned =
      add
        .assignment[Int]("a=")
        .assignment[Int]("b=")
        .apply(_ + _)

    val shiftArgs =
      shift
        .argument[String]
        .argument[Int]
        .apply { (a, b) =>
          a + b
        }

    val shiftOpt =
      shift
        .option("-a" | "--a")
        .apply(() => println("HA"))


    val store = Store.empty +> addUnnamed +> addNamed +> addAssigned +> shiftArgs +> shiftOpt

    val params1 = List("add", "2", "3")
    val params2 = List("add", "a", "2", "b", "3")
    val params3 = List("add", "a=2", "b=3")
    val params4 = List("shift", "a", "1")
    val params5 = List("shift", "-a")

    //val params6 = List("add", "--help")
    println(Interpreter.interpret(store).run(params4))
  }
}
