package core


import scala.util.{Failure, Success, Try}
import scalaz.{IndexedState, State, ValidationNel, \/}
import scalaz.syntax.validation._
import Interpreter._
import scala.language.postfixOps
import scalaz.syntax.either._

object Interpreter {
  type Result[A] = ValidationNel[Throwable, A]

  def run[A](runner: Runner[A]): List[String] => Result[A] = input => {
    (SyntaxChecker(runner) andThen TypeChecker(runner)) (input)
  }
}

object SyntaxChecker {
  def apply[A](runner: Runner[A]): List[String] => Result[Inter[(Sym, String)]] = input => {
    if (input.length != runner.syntax.depth) {
      new Throwable("Wrong argument arity: Insufficient or superfluous number of arguments").failureNel
    }
    else {
      val zipped = runner.syntax zipL input
      Inter.validate(zipped)(interpret) map (_ => zipped)
    }
  }

  def interpret(sym: Sym, input: String): \/[Throwable, String] = sym match {
    case Named(label) =>
      if (label == input) input.right
      else new Throwable(s"Input of `$input` does not match expected `$label`").left
    case Type => input.right
    case Assign(label, op) =>
      if (input startsWith (label + op)) input.right
      else new Throwable(s"Inproper assignment in `$label`").left
  }
}

object TypeChecker {
  def apply[A, C](runner: Runner[A]): Result[Inter[(Sym, String)]] => Result[A] = syntax => {
    syntax flatMap (normalise _ andThen runner.run)
  }

  def normalise(syntax: Inter[(Sym, String)]): List[String] = {
    syntax filterL (_._1 isTyped) map {
      case ((Assign(l, op), input)) => (input split op) (1)
      case (_, input) => input
    }
  }
}

object Types {
  type TypedParams[A] = IndexedState[List[String], List[String], Result[A]]

  def morph[A](f: String => A): TypedParams[A] =
    for {
      params <- State.get[List[String]]
      _ <- State.modify[List[String]](_.tail)
    } yield {
      Try(f(params.head)) match {
        case Success(t) => t.successNel
        case Failure(e) => e.failureNel
      }
    }
}
