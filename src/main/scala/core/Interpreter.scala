package core

import scalaz.syntax.validation._
import scalaz.Kleisli
import Validators._
import core.Store.MapT
import Store._
import Binary.treeSyntax
import core.Man.HelpConfig
import core.Read._

import scalaz.syntax.applicative._

sealed trait Phase[A] {
  def fold[B](f: Result[A] => B)(g: String => B): B = this match {
    case Interpretation(r) => f(r)
    case Meta(a) => g(a)
  }
}

case class Interpretation[A](result: Result[A]) extends Phase[A]

case class Meta[A](data: String) extends Phase[A]

//TODO: Can't I generalise this? Or should'nt I generalise this?
object Interpreter {
  type Step[A, B] = Kleisli[Result, A, B]
  type IAST = Tree[(Denot, String)] // interpolated abstract syntax tree

  def partialMatch(commands: Set[Tree[Denot]], input: List[String]): Set[Tree[Denot]] =
    commands.filter {
      _.zipL(input).
        serialise.
        forall {
          case (Identifier(sym, _, _), value) => sym.isSymbol(value)
          case (TypedIdentifier(sym, _, _), value) => sym.find(a => value startsWith a).fold(false)(_ => true)
          case (Typing(p, _), value) => p(value).isSuccess
        }
    }

  def step[A, B](f: A => Result[B]): Step[A, B] = Kleisli[Result, A, B](f)

  def phase[A, B](f: A => Phase[B]) = Kleisli[Phase, A, B](f)

  def interpret[A](cli: Cli[A]): Step[List[String], A] = interpret(cli.store)

  def interpret[A](store: Store[Store.MapT, A]): Step[List[String], A] = resolve(store.keySet) andThen runFrom(store)

  def interpretH[A](store: Store[Store.MapT, A], helpConfig: HelpConfig = HelpConfig(150, 5, 5)) = {
    lazy val help = Man.helper(store, helpConfig)
    lazy val suggest = Man.suggester(store, helpConfig)
    phase { (input: List[String]) =>
      input match {
        case args if args.last == "--help" => Meta[A](help(input.dropRight(1)))
        case args if args.last == "--sgst" => Meta[A](suggest(input.dropRight(1)))
        case _ => Interpretation[A] {
          interpret(store).run(input)
        }
      }
    }
  }

  def interpret[A](command: Cmd[A]): Step[List[String], A] = step { input =>
    val x = command.syntax zipL input

    (x.validate(syntax) |@| x.validate(types) |@| run(command, x)) {
      (_, _, a) => a
    }
  }

  def resolve(keySet: Set[Tree[Denot]]) =
    fit(keySet) andThen
      validate(syntax) andThen
      validate(types) andThen
      select

  def fit(commands: Set[Tree[Denot]]) = step { (input: List[String]) =>
    commands.filter {
      case tree@Identifier(Label(value), _, _) -< (_, _) => (value == input.head) && tree.depth == input.size
    }
      .map(_ zipL input)
      .toList
      .successNel
  }

  def validate(f: ((Denot, String)) => Result[(Denot, String)]) = step { (is: List[IAST]) =>
    val point = List.empty[IAST].successNel[Throwable]
    is.map(_ validate f)
      .filter(_.isSuccess)
      .foldRight(point)((a, b) => (a |@| b) (_ :: _))
  }

  def select = step { (rem: List[IAST]) =>
    rem match {
      case h :: Nil => h.successNel
      case Nil => new Throwable("Could not find any command matching that input").failureNel
      case _ => new Throwable("Ambiguous input. Too many commands match the input").failureNel
    }
  }

  def runFrom[A](m: Store[MapT, A]) = step { (syntax: IAST) =>
    val key = syntax map (_._1)
    val fail = new Throwable("Unknown command").failureNel[A]
    m.get(key).fold(fail)(run(_, syntax))
  }

  def run[A](cmd: Cmd[A], syntax: IAST): Result[A] = {
    val args = syntax.filterL(_._1.isTyped).map(_._2)
    cmd.run(args)
  }
}

object Validators {

  def syntax(assoc: (Denot, String)) = (assoc match {
    case (Identifier(symbol, _, _), input) => symbol.find(_ == input)
    case (TypedIdentifier(symbol, _, _), input) => symbol.find(v => input startsWith v)
    case _ => Some(Label(""))
  }).fold {
    new Throwable(s"Input of `${assoc._2}` does not match the expected input of ${assoc._1.show}").failureNel[(Denot, String)]
  } { _ => assoc.successNel }


  def types(assoc: (Denot, String)) = assoc match {
    case (Typing(proof, _), input) => proof(input).map(_ => assoc)
    case (TypedIdentifier(_, proof, _), input) => proof(input).map(_ => assoc)
    case _ => assoc.successNel
  }
}

