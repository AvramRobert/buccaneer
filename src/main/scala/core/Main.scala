package core

import Implicits._
import Binary.treeSyntax
import Store._
import core.Man.HelpConfig

object Main {
  def main(args: Array[String]) = {
    val add = Command("add").msg("Add command")
    val shift = Command("shift")

    val addUnnamed =
      add
        .argument[Int].msg("First argumentaksdkjadkjashkjsksjdaskjasdnksadksajnaskdskadjas")
        .argument[Int].msg("Second argument")
        .apply(_ + _)

    val addUnnamedRec =
      add.
        option("-r").
        argument[Int].msg("First argumentaksdkjadkjashkjsksjdaskjasdnksadksajnaskdskadjas").
        argument[Int].msg("Second argument").
        apply(_ + _)

    val addNamed =
      add
        .option("a").msg("Argument \"a\"").argument[Int]
        .option("b").msg("Argument \"b\"").argument[Int]
        .apply(_ + _)

    val addAssigned =
      add
        .assignment[Int]("a=")
        .assignment[Int]("b=")
        .apply(_ + _)

    val shiftArgs =
      shift
        .argument[String].msg("Some string argument")
        .argument[Int].msg("Some int argument")
        .apply { (a, b) =>
          a + b
        }

    val shiftOpt =
      shift
        .option("-a" | "--a")
        .apply(() => println("HA"))

    val store = Store.empty +> addUnnamed +> addUnnamedRec +> addNamed +> addAssigned +> shiftArgs +> shiftOpt

    //    val params1 = List("add", "2", "3")
    //    val params2 = List("add", "a", "2", "b", "3")
    //    val params3 = List("add", "a=2", "b=3")
    //    val params4 = List("shift", "a", "1")
    //    val params5 = List("shift", "-a")
    //println(Interpreter.interpret(store).run(params4))

    Interpreter.
      interpretH(store).
      run(List("add", "1", "--help")).
      fold(_ => println(""))(println)
  }

}