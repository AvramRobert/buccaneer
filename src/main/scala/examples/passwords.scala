package examples

import core.everything._
import core.Implicits._
import scala.util.Random.{nextInt, nextLong, setSeed, shuffle}
import scala.collection.immutable.Stream.continually

object passwords {
  val help = option("-h", "--help").msg("Prints this page")
  val suggest = option("-s", "--suggest").msg("Prints a suggestion list")
  val empty = argument[Unit].msg("No parameters. Runs the command with default params.")
  val seed = assignment[Long]("seed=").msg("A seed, that the user can explicitly specify")
  val min = assignment[Int]((i: Int) => i >= 5)("min=", "m=").msg("Minimal password length. Values lower than 5 are not accepted")
  val max = assignment[Int]((i: Int) => i >= 5)("max=", "x=").msg("Maximal password length. Values lower than 5 are not accepted")
  val exclusion = assignment[String]("exclude=").msg("String of characters that should be excluded from the creation process")
  val version = option("-v", "--version").msg("Outputs the current version")

  val config = manpage(
    programName = "passwords",
    programDescription = "Dummy password generator",
    help = help,
    suggest = suggest)

  lazy val dictionary: Vector[Char] = (33 to 127).map(_.toChar).toVector

  def exclude(exclusion: String): Vector[Char] = dictionary diff exclusion

  def pickMin(max: Int): Int = {
    val options = (0 to max).filter(_ < max)
    options(nextInt(options.size))
  }

  def pickMax(min: Int): Int = {
    val options = (min to (min + 10)).filter(_ > min)
    options(nextInt(options.size))
  }

  val passwords = Cli(
    version { () => List("Passwords 0.1.0") },
    empty { _ => generate() },
    exclusion { ex => generate(dictionary = exclude(ex)) },
    seed { seed => generate(seed = seed) },
    max { max => generate(minLength = pickMin(max), maxLength = max) },
    min { min => generate(minLength = min, maxLength = pickMax(min)) },
    (min - max) { (min, max) => generate(minLength = min, maxLength = max) },
    (max - seed) { (max, seed) => generate(minLength = pickMin(max), maxLength = max, seed = seed) },
    (min - seed) { (min, seed) => generate(minLength = min, maxLength = pickMax(min), seed = seed) },
    (seed - min) { (seed, min) => generate(minLength = min, maxLength = pickMax(min), seed = seed) },
    (seed - max) { (seed, max) => generate(minLength = pickMin(max), maxLength = max, seed = seed) },
    (exclusion - min) { (ex, min) => generate(minLength = min, maxLength = pickMax(min), dictionary = exclude(ex)) },
    (exclusion - max) { (ex, max) => generate(minLength = pickMin(max), maxLength = max, dictionary = exclude(ex)) },
    (max - exclusion) { (max, ex) => generate(minLength = pickMin(max), maxLength = max, dictionary = exclude(ex)) },
    (min - exclusion) { (min, ex) => generate(minLength = min, maxLength = pickMax(min), dictionary = exclude(ex)) },
    (min - max - seed) { (min, max, seed) => generate(minLength = min, maxLength = max, seed = seed) },
    (seed - min - max) { (seed, min, max) => generate(minLength = min, maxLength = max, seed = seed) },
    (min - max - exclusion) { (min, max, ex) => generate(minLength = min, maxLength = max, dictionary = exclude(ex)) },
    (exclusion - min - max) { (ex, min, max) => generate(minLength = min, maxLength = max, dictionary = exclude(ex)) },
    (min - max - exclusion - seed) { (min, max, ex, seed) =>
      generate(minLength = min, maxLength = max, seed = seed, dictionary = exclude(ex))
    },
    (min - max - seed - exclusion) { (min, max, seed, ex) =>
      generate(minLength = min, maxLength = max, seed = seed, dictionary = exclude(ex))
    },
    (seed - exclusion - min - max) { (seed, ex, min, max) =>
      generate(minLength = min, maxLength = max, seed = seed, dictionary = exclude(ex))
    },
    (exclusion - seed - min - max) { (ex, seed, min, max) =>
      generate(minLength = min, maxLength = max, seed = seed, dictionary = exclude(ex))
    })

  def generate(minLength: Int = 8,
               maxLength: Int = 9,
               seed: Long = nextLong(),
               dictionary: Vector[Char] = dictionary): List[String] = {
    val range = (minLength until maxLength).toVector
    setSeed(seed)
    continually(
      shuffle(dictionary).
        take(range(nextInt(maxLength - minLength)))).
      take(5).
      map(_.mkString("")).
      toList
  }

  def main(args: Array[String]): Unit = {
      Interpreter.
        interpretH(passwords, config).
        run(args.toList).
        foreach(_ foreach println)
  }
}