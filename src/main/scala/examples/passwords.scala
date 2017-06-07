package examples

import core.DSL._
import core.Cli
import core.Interpreter._
import core.Implicits._

import scala.util.Random

object passwords {

  val unit = argument[Unit]

  val int = argument[Int]
  val string = argument[String]
  val idx = argument[Int]((i: Int) => i == 0 || i == 1)
  val algorithm = option("-a")
  val seed = option("-s")
  val min = option("-m")
  val max = option("-x")
  val version = option("-v")
  val help = option("-h")
  val exclude = option("-E")

  lazy val dictionary: Vector[Char] = (33 to 127).map(_.toChar).toVector

  def exclude(whitelist: Vector[Char]): Vector[Char] = dictionary diff whitelist

  def exclude(exclusion: String): Vector[Char] = exclude(exclusion.toCharArray.toVector)

  def pick(p: Int => Boolean): Int = {
    val options = (5 to 20).filter(p)
    options(Random.nextInt(options.size))
  }

  /*
   TODO: It would be quite nice if I could define compound denotations and use them as such
    Ex:
    val int = argument[Int]
    val min = option("-m") and int
    ...
    (.. - min - ..) { .. }
   */
  val apg = Cli(
    version { () => List("Dummy passwords 0.1.0") },
    unit { _ => generate() },
    (exclude - string) { ex => generate(dictionary = exclude(ex)) },
    (seed - int) { seed => generate (seed = seed) },
    (max - int) { max => generate(maxLength = max) },
    (min - int) { min => generate(minLength = min) },
    (min - int - max - int) { (min, max) => generate(minLength = min, maxLength = max) },
    (min - int - max - int - seed - int) { (min, max, seed) => generate(minLength = min, maxLength = max, seed = seed) },
    (seed - int - min - int - max - int) { (seed, min, max) => generate(minLength = min, maxLength = max, seed = seed) },
    (min - int - max - int - exclude - string) { (min, max, ex) => generate(minLength = min, maxLength = max, dictionary = exclude(ex)) },
    (algorithm - idx) { idx => generate(idx) },
    (algorithm - idx - min - int) { (idx, min) => generate(idx, min, pick(_ > min)) },
    (algorithm - idx - max - int) { (idx, max) => generate(idx, pick(_ < max), max) },
    (algorithm - idx - min - int - max - int) { (idx, min, max) => generate(idx, min, max) },
    (algorithm - idx - min - int - seed - int) { (idx, min, seed) => generate(idx, min, pick(_ > min), seed) },
    (algorithm - idx - max - int - seed - int) { (idx, max, seed) => generate(idx, pick(_ < max), max, seed) },
    (algorithm - idx - seed - int - min - int) { (idx, seed, min) => generate(idx, min, pick(_ > min), seed) },
    (algorithm - idx - seed - int - max - int) { (idx, seed, max) => generate(idx, pick(_ < max), max, seed) },
    (algorithm - idx - min - int - max - int - seed - int) { (idx, min, max, seed) => generate(idx, min, max, seed) },
    (algorithm - idx - min - int - max - int - exclude - string) { (idx, min, max, ex) =>
      generate(idx, min, max, dictionary = exclude(ex))
    },
    (algorithm - idx - exclude - string - min - int - max - int) { (idx, ex, min, max) =>
      generate(idx, min, max, dictionary = exclude(ex))
    },
    (algorithm - idx - min - int - seed - int - exclude - string) { (idx, min, seed, ex) =>
      generate(idx, min, pick(_ > min), seed, exclude(ex))
    },
    (algorithm - idx - max - int - seed - int - exclude - string) { (idx, max, seed, ex) =>
      generate(idx, pick(_ < max), max, seed, exclude(ex))
    },
    (algorithm - idx - exclude - string - min - int - seed - int) { (idx, ex, min, seed) =>
      generate(idx, min, pick(_ > min), seed, exclude(ex))
    },
    (algorithm - idx - exclude - string - max - int - seed - int) { (idx, ex, max, seed) =>
      generate(idx, pick(_ < max), max, seed, exclude(ex))
    },
    (algorithm - idx - seed - int - exclude - string - max - int) { (idx, seed, ex, max) =>
      generate(idx, pick(_ < max), max, seed, exclude(ex))
    },
    (algorithm - idx - seed - int - exclude - string - min - int) { (idx, seed, ex, min) =>
      generate(idx, min, pick(_ > min), seed, exclude(ex))
    },
    (algorithm - idx - exclude - string - seed - int - min - int) { (idx, ex, seed, min) =>
      generate(idx, min, pick(_ > min), seed, exclude(ex))
    },
    (algorithm - idx - exclude - string - seed - int - max - int) { (idx, ex, seed, max) =>
      generate(idx, pick(_ < max), max, seed, exclude(ex))
    }

    //(seed - int - algorithm - idx - min - int - max - int) { (seed, index, min, max) => generate(index, min, max, seed) },
  )

  def generate(algorithmIndex: Int = 0,
               minLength: Int = 8,
               maxLength: Int = 9,
               seed: Int = Random.nextInt(),
               dictionary: Vector[Char] = dictionary): List[String] = {
    List(s"Algorithm: $algorithmIndex, min: $minLength, max: $maxLength, seed: $seed, dictSize: ${dictionary.size}")
  }

  implicit class ArgsOps(s: String) {
    def args: List[String] = s.split(" ").toList
  }

  def main(args: Array[String]): Unit =
    interpretH(apg).
      run("-a 1 -E abc -m 2 -x 5".args).
      foreach(_ foreach println)
}