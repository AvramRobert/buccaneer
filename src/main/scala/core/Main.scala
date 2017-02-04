package core

import core.Implicits._
import core.DSL._
import core.Store._

object Main {
  def main(args: Array[String]) = {
    val add = command("add").msg("The add command")
    val shift = command("shift").msg("The shift command")
    val left = command("left").msg("The left subcommand")

    val int = argument[Int].msg("Integer argument")
    val bool = argument[Boolean].msg("Boolean argument")
    val a = option("a")
    val b = option("b")
    val r = option("-r" | "--r")

    val addUnnamed = (add - int - int) (_ + _)
    val addNamed = (add - a - int - b - int) (_ + _)
    val addAssigned = (add - assignment[Int]("a") - assignment[Int]("b")) (_ + _)
    val addUnnamedRec = (add - r - int - int) (_ + _)
    val shiftArgs = (shift - bool - argument[String]) ((_, _) => println("YA"))
    val shiftOpt = (shift - r - bool) (x => println(x))

    val shiftLeft = (shift - left - bool) (x => println(x))

    val store = Store.empty +> addUnnamed +> addUnnamedRec +> addNamed +> addAssigned +> shiftArgs +> shiftOpt +> shiftLeft

    Interpreter.
      interpret(addNamed).
      run(List("add", "a", "1", "b", "1")).
      fold(println)(_ foreach println)(println)

//    Interpreter.
//      interpretH(store).
//      run(List("add", "a", "2", "--sgst")).
//      fold(println)(_ foreach println)(println)
  }
}