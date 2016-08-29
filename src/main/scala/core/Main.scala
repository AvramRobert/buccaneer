package core

import Implicits._
import Store._

/*
 * TODO: Are type safe partial HMaps possible?
 * Apparently shapeless Records might be able to do what I want with partial HMaps
 * val records = addNamed.syntax ->> addNamed :: addAssigned.syntax ->> addAssigned :: shift.syntax ->> shift :: HNil
 * records.get(addNamed.syntax) => should be a Runner[A]
 * Problems: implicits
 */

object Main {
  def main(args: Array[String]) = {

    def command(label: String, desc: String = "") = Command(label, desc)

    val add = command("add", "This is the add command")

    val addUnnamed =
      add
        .unnamed[Int]
        .unnamed[Int]
        .apply(_ + _)

    val addNamed =
      add
        .named[Int]("a", desc = "Named parameter named `a`")
        .named[Int]("b", desc = "Named parameter named `b`")
        .apply(_ + _)

    val addAssigned =
      add
        .assignment[Int]("a", ":", desc = "Assignment with `:` named `a`")
        .assignment[Int]("b", ":", desc = "Assignment with `:` named `b`")
        .apply(_ + _)

    val shift =
      command("shift", desc = "Command shift")
        .unnamed[String]
        .unnamed[Int]
        .apply { (a, b) =>
          a + b
        }

    val opt = {
      add
        .option("a", desc = "Option of for `add`")
        .apply { _ =>
          println("YAY")
        }
    }

    val store = Store.empty +> addUnnamed +> addNamed +> addAssigned

    val params1 = List("add", "2", "3")
    val params2 = List("add", "a", "2", "b", "3")
    val params3 = List("add", "a:2", "b:3")
    val params4 = List("shift", "a", "1")
    val params5 = List("subtract", "5", "4")
    val params6 = List("add", "--help")


    println(Interpreter.interpretH(store).run(params6))
  }

}
