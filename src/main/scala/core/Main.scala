package core

import core.Implicits._
import core.DSL._

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

    val shiftLeftOp = (shift - left - r - bool) (x => println(x))

    val shiftLeftOp2 = (shift - left - r - a)(() => "ja")
    val cli = Cli(addUnnamed, addUnnamedRec, addNamed, addAssigned, shiftArgs, shiftOpt, shiftLeft, shiftLeftOp, shiftLeftOp2)

println(shiftLeft.syntax.string(" ")(_.show))
//    Interpreter.
//      interpret(addNamed).
//      run(List("add", "a", "1", "b", "1")).
//      fold(println)(_ foreach println)(println)

    Interpreter.
      interpretH(cli).
      run(List("add", "--help")).
      fold(println)(_ foreach println)(println)

  }
}