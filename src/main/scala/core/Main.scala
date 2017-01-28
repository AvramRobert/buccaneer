package core

import Implicits._
import Store._

object Main {
  def main(args: Array[String]) = {
    import Command._


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
      interpretH(store).
      run(List("shift", "x", "--help")).
      fold(errors => println(errors))(value => println(value))(metadata => println(metadata))

  }

  /*
   TODO: Problem!

   Everything fails when I input something that is not a valid command, but request to see help or suggestions.
   => That's because the partial match doesn't work properly. If at some point the partial match encounters something
   it doesn't know, it leads to an empty Set Tree[Denot], which makes everything blow up.
   */
}