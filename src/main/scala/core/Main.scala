package core

import core.Interpreter._

object Main {
  def main(args: Array[String]) = {

    def command(label: String) = Command(label)

    implicit val map: Mapper[String, Int] = Mapper[String, Int](_.toInt)
    implicit def mapId[A]: Mapper[A, A] = Mapper[A, A](identity)

    val z =
      command("add")
        .named[Int]("a")
        .named[Int]("b")
        .apply(_ + _)

    val params = List("add", "a", "2", "b", "3")
    val result = run(z)(params)

    println(result)
  }
}
