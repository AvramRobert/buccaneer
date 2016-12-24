package core

import Implicits._
import Store._

import scalaz.State

object Main {
  def main(args: Array[String]) = {

    //    val add = Command("add").msg("Add command")
    //    val shift = Command("shift")
    //
    //    val addUnnamed =
    //      add
    //        .argument[Int].msg("First argument")
    //        .argument[Int].msg("Second argument")
    //        .apply(_ + _)
    //
    //    val addNamed =
    //      add
    //        .option("a").msg("Argument \"a\"").argument[Int]
    //        .option("b").msg("Argument \"b\"").argument[Int]
    //        .apply(_ + _)
    //
    //    val addAssigned =
    //      add
    //        .assignment[Int]("a=")
    //        .assignment[Int]("b=")
    //        .apply(_ + _)
    //
    //    val shiftArgs =
    //      shift
    //        .argument[String]
    //        .argument[Int]
    //        .apply { (a, b) =>
    //          a + b
    //        }
    //
    //    val shiftOpt =
    //      shift
    //        .option("-a" | "--a")
    //        .apply(() => println("HA"))
    //
    //
    //    val store = Store.empty +> addUnnamed +> addNamed +> addAssigned +> shiftArgs +> shiftOpt
    //
    //    val params1 = List("add", "2", "3")
    //    val params2 = List("add", "a", "2", "b", "3")
    //    val params3 = List("add", "a=2", "b=3")
    //    val params4 = List("shift", "a", "1")
    //    val params5 = List("shift", "-a")

    import Formatter._

    implicit val f: Lexical[Char] = new Lexical[Char] {
      override def blank = ' '

      override def break = '\n'

      override def continuation = '-'

      override def eq(a1: Char, a2: Char) = a1 == a2
    }

    def textFormatter(text: String): Formatter[Char] = formatter(text.toCharArray.toVector)


    val x = textFormatter("Hello, my name is Robert")
    val y = textFormatter("Well hello Robert")
    val z = textFormatter("askdlksajlskjdlkasjdskdj")

    println(y.ofWidth(5).run.mkString(""))
//    println(
//      y.ofWidth(5)
//        .continuous
//        .align(z.ofWidth(5).continuous, 1)
//        .align(x.ofWidth(7).continuous, 4)
//        .run
//        .mkString(""))
    //val params6 = List("add", "--help")
    //println(Interpreter.interpret(store).run(params4))
  }
}
