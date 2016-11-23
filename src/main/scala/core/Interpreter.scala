package core

import scalaz.syntax.validation._
import scalaz.Kleisli
import Validators._
import core.Store.MapT
import Store._
import Binary.treeSyntax
import core.Reified._
import scalaz.syntax.applicative._

//TODO: Can't I generalise this? Or should'nt I generalise this?
object Interpreter {
  type Step[A, B] = Kleisli[Result, A, B]
  type IAST = Tree[(Denot, String)] // interpolated abstract syntax tree

  def step[A, B](f: A => Result[B]): Step[A, B] = Kleisli[Result, A, B](f)

  def interpret[A](cli: Cli[A]): Step[List[String], A] = interpret(cli.store)

  def interpret[A](store: Store[Store.MapT, A]): Step[List[String], A] = resolve(store.keySet) andThen runFrom(store)

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

  //  def help[A](store: Store[Store.MapT, A]): Store[MapT, Any] = {
  //    store.keySet
  //      .filter(_.rootOf(_.isCommand))
  //      .map {
  //        _.rootOption
  //          .fold(Runner(Command.runnerState(_ => ()), Binary.lift(Sym.named("", "")))) { com =>
  //            val syntax = Binary.lift(com) affix Sym.named("--help", "Help")
  //            val f = Command.runnerState[Unit] { _ =>
  //              println(Man.buildFor(store.keySet.toList, com))
  //            }
  //            Runner(f, syntax)
  //          }
  //      }
  //      .foldLeft(Store.widen(store))(_ +> _)
  //  }

  def fit(commands: Set[Tree[Denot]]) = step { (input: List[String]) =>
    commands.filter {
      case tree@Identifier(Label(value), _) -< (_, _) => (value == input.head) && tree.depth == input.size
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
  def syntax(assoc: (Denot, String)) = assoc._1 match {
    case Identifier(symbol, _) => symbol.find(_ == assoc._2) match {
      case Some(_) => assoc.successNel
      case None => new Throwable(s"Input of `${assoc._2}` does not match the expected input of ${symbol.show}").failureNel
    }
    case TypedIdentifier(symbol, _, _) => symbol.find(v => assoc._2 startsWith v) match {
      case Some(_) => assoc.successNel
      case None => new Throwable(s"Prefix of `${assoc._2}` does not match the expected prefix of ${symbol.show}<value>").failureNel
    }
    case _ => assoc.successNel
  }

  def types(assoc: (Denot, String)) = assoc._1 match {
    case Typing(proof, _) => proof(assoc._2).map(_ => assoc)
    case TypedIdentifier(_, proof, _) => proof(assoc._2).map(_ => assoc)
    case _ => assoc.successNel
  }
}

