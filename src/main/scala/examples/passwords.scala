package examples

import core.DSL._
import core.Cli
import core.Interpreter._
import core.Implicits._
import scala.util.Random.{shuffle, nextInt, nextLong, setSeed}
import scala.collection.immutable.Stream.continually

object passwords {

  val config = manConfig(programName = "passwords", programDescription = "Dummy password generator")
  val empty = argument[Unit]
  val seed = assignment[Long]("seed=").msg("A seed, that the user can explicitly specify")
  val min = assignment[Int]("min=", (i: Int) => i >= 5).msg("Minimal password length. Values lower than 5 are not accepted")
  val max = assignment[Int]("max=", (i: Int) => i >= 5).msg("Maximal password length. Values lower than 5 are not accepted")
  val exclusion = assignment[String]("exclude=").msg("String of characters that should be excluded from the creation process")
  val version = option("-v" | "--version").msg("Outputs the current version")

  lazy val dictionary: Vector[Char] = (33 to 127).map(_.toChar).toVector

  def exclude(exclusion: String): Vector[Char] = dictionary diff exclusion

  def pick(p: Int => Boolean): Int = {
    val options = (5 to 20).filter(p)
    options(nextInt(options.size))
  }

  val apg = Cli(
    version { () => List("Passwords 0.1.0") },
    empty { _ => generate() },
    exclusion { ex => generate(dictionary = exclude(ex)) },
    seed { seed => generate(seed = seed) },
    max { max => generate(maxLength = max) },
    min { min => generate(minLength = min) },
    (min - max) { (min, max) => generate(minLength = min, maxLength = max) },
    (max - seed) { (max, seed) => generate(maxLength = max, seed = seed) },
    (min - seed) { (min, seed) => generate(minLength = min, seed = seed) },
    (seed - min) { (seed, min) => generate(minLength = min, seed = seed) },
    (seed - max) { (seed, max) => generate(maxLength = max, seed = seed) },
    (exclusion - min) { (ex, min) => generate(minLength = min, dictionary = exclude(ex)) },
    (exclusion - max) { (ex, max) => generate(maxLength = max, dictionary = exclude(ex)) },
    (max - exclusion) { (max, ex) => generate(maxLength = max, dictionary = exclude(ex)) },
    (min - exclusion) { (min, ex) => generate(minLength = min, dictionary = exclude(ex)) },
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

  def main(args: Array[String]): Unit =
    interpretH(apg, config).
      run(args.toList).
      foreach(_ foreach println)

}