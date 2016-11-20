package core

import scalaz.syntax.validation._
import scalaz.{Bind, Kleisli, ValidationNel, \/}
import scalaz.syntax.either._
import Validators._
import core.Store.MapT
import Store._
import Binary.treeSyntax
import scalaz.syntax.applicative._

//TODO: Can't I generalise this? Or should'nt I generalise this?
object Interpreter {
  type Slight[A] = ValidationNel[Throwable, A]
  type Step[A, B] = Kleisli[Slight, A, B]

  implicit val bindSlight: Bind[Slight] = new Bind[Slight] {
    override def bind[A, B](fa: Slight[A])(f: (A) => Slight[B]): Slight[B] = fa flatMap f

    override def map[A, B](fa: Slight[A])(f: (A) => B): Slight[B] = fa map f
  }

  def step[A, B](f: A => Slight[B]): Step[A, B] = Kleisli[Slight, A, B](f)

  def interpret[A](cli: Cli[A]): Step[List[String], A] = interpret(cli.store)

  def interpret[A](store: Store[Store.MapT, A]): Step[List[String], A] = resolve(store.keySet) andThen run(store)

  //  def interpretH[A](store: Store[Store.MapT, A]): Step[List[String], Any] = {
  //    val ns = help(store)
  //    interpret(ns)
  //  }

  def interpret[A](runner: Cmd[A]): Step[List[String], A] = interpret(Store.empty + runner)

  def interpret2[A](command: Cmd[A]): Step[List[String], A] = step { list =>
    val x = command.syntax zipL list
    (x.validate(syntax) |@| x.validate(types) |@| command.run(list)) {
      (_, _, a) => a
    }
  }

  def resolve(keySet: Set[Tree[Denot]]) =
    shape(keySet) andThen
      validate(syntax) andThen
      validate(types) andThen
      narrow

  //  def help[A](store: Store[Store.MapT, A]): Store[MapT, Any] = {
  //    store
  //      .keySet
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

  def shape(commands: Set[Tree[Denot]]) = step { (input: List[String]) =>
    commands
      .filter {
        case tree @ Identifier(Label(value)) -< (l, r) => (value == input.head) && tree.depth == input.size
      }
      .map(_ zipL input)
      .toList
      .successNel
  }

  def validate(f: ((Denot, String)) => Throwable \/ (Denot, String)) = step { (is: List[Tree[(Denot, String)]]) =>
    lazy val point = List.empty[Tree[(Denot, String)]].successNel[Throwable]
    is
      .map(_ validate f)
      .filter(_.isSuccess)
      .foldRight(point)((a, b) => (a |@| b) (_ :: _))
  }

  def narrow = step { (rem: List[Tree[(Denot, String)]]) =>
    rem match {
      case h :: Nil => h.successNel
      case h :: t => new Throwable("Ambiguous command. Too many commands match the input").failureNel
      case Nil => new Throwable("Input is not a valid command").failureNel
    }
  }

  def run[A](m: Store[MapT, A]) = step { (syntax: Tree[(Denot, String)]) =>
    val key = syntax map (_._1)
    m.get(key)
      .fold(new Throwable("Unknown command").failureNel[A]) { command =>
        command.run(syntax filterL (_._1.isTyped) map (_._2))
      }
  }
}

object Validators {
  def syntax(assoc: (Denot, String)) = assoc._1 match {
    case Identifier(symbol) => symbol.find(_ == assoc._2) match {
      case Some(_) => assoc.right
      case None => new Throwable(s"Input of `${assoc._2}` does not match the expected input of ${symbol.show}").left
    }
    case TypedIdentifier(symbol, _) => symbol.find(v => assoc._2 startsWith v) match {
      case Some(_) => assoc.right
      case None => new Throwable(s"Input of `${assoc._2}` does not match the expected input of ${symbol.show}").left
    }
    case _ => assoc.right
  }

  def types(assoc: (Denot, String)) = assoc._1 match {
    case Typing(proof) =>
      proof(assoc._2)
        .leftMap(_ => new Throwable(s"Could not prove type of `${assoc._2}`"))
        .map(_ => assoc)
    case TypedIdentifier(_, proof) =>
      proof(assoc._2)
        .leftMap(_ => new Throwable(s"Could not prove type of `${assoc._2}`"))
        .map(_ => assoc)
    case _ => assoc.right
  }
}

