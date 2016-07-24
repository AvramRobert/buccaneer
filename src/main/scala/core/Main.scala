package core

import core.Interpreter._

object Main {
  def main(args: Array[String]) = {

    def command(label: String) = Command(label)

    implicit val map: Mapper[String, Int] = Mapper[String, Int](_.toInt)
    implicit def mapId[A]: Mapper[A, A] = Mapper[A, A](identity)

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

    val params1 = List("add", "2", "3")
    val params2 = List("add", "a", "2", "b", "3")
    val params3 = List("add", "a:2", "b:3")
    val result1 = run(addUnnamed)(params1)
    val result2 = run(addNamed)(params2)
    val result3 = run(addAssigned)(params3)

    println(result1)
    println(result2)
    println(result3)
  }
}
