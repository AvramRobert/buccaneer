package core

import shapeless.HMap

/*
 * TODO: Problems
 * Given that the `Runner[A]` is invariant and inferred specifically in its `A`, Scala will not resolve them to `Any`
 * when `Runner`s of different types are stored in a `Map`. Even if a type parameter is added to the `run` function,
 * the inferred type will be some union of these things, which is not specific enough in order to be accepted.
 * As such, I have to resort to an HMap, that throws away all type information by means of indirection and casting.
 * This forgets the concrete `A` when stored in the `Map`, and casts it to an `Any` when it gets resolved.
 * The compiler accepts this indirection and both compiles and runs the commands appropriately.
 *
 * I do however need to find a more elegant way of doing this. The first version of this library will be rather
 * not so type safe when actually running the commands. It will be type safe in their definition however.
 * I should try to find a way to index and traverse different types within a structure.
 * This will be however something that I don't necessarily have to do in the first version.
 *
 * TODO: Are type safe partial HMaps possible?
 * Whereby the key is not polymorphic, but the values are HLists?
 */

object Main {
  def main(args: Array[String]) = {

    def command(label: String) = Command(label)

    implicit val intR: Reified[Int] = Reified(_.toInt)
    implicit val intS: Reified[String] = Reified(identity)

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


    val nm = CMap.typedMap + addNamed + addUnnamed + addAssigned
    val nm2 = CMap.untypedMap + addNamed + addUnnamed + shift

    println(Interpreter.interpret(nm2).run(params3))
  }
}
