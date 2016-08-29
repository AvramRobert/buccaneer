package core

import scala.util.{Failure, Success, Try}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scalaz.{Bind, Kleisli, ValidationNel, \/}
import scalaz.syntax.either._
import Validators._
import core.Store.MapT
import Store._
import Binary.treeSyntax

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

  def interpretH[A](store: Store[Store.MapT, A]): Step[List[String], Any] = {
    val ns = help(store)
    interpret(ns)
  }

  def interpret[A](runner: Runner[A]): Step[List[String], A] = interpret(Store.empty + runner)

  def resolve(keySet: Set[Tree[Sym]]) =
    shape(keySet) andThen
      validate(syntax) andThen
      validate(types) andThen
      narrow

  def help[A](store: Store[Store.MapT, A]): Store[MapT, Any] = {
    store
      .keySet
      .filter(_.rootOf(_.isCommand))
      .map {
        _.rootOption
          .fold(Runner(Command.runnerState(_ => ()), Binary.liftTree(Sym.named("", "")))) { com =>
            val syntax = Binary.liftTree(com) affix Sym.named("--help", "Help")
            val f = Command.runnerState[Unit] { _ =>
              println(Man.buildFor(store.keySet.toList, com))
            }
            Runner(f, syntax)
          }
      }
      .foldLeft(Store.widen(store))(_ +> _)
  }

  def shape(commands: Set[Tree[Sym]]) = step { (input: List[String]) =>
    commands
      .filter(tree => tree.rootOf(_.isCommand) && tree.depth == input.size)
      .map(_ zipL input)
      .toList
      .successNel
  }

  def validate(f: ((Sym, String)) => Throwable \/ (Sym, String)) = step { (is: List[Tree[(Sym, String)]]) =>
    lazy val point = List.empty[Tree[(Sym, String)]].successNel[Throwable]
    is
      .map(_ validate f)
      .filter(_.isSuccess)
      .foldRight(point)((a, b) => (a |@| b) (_ :: _))
  }

  def narrow = step { (rem: List[Tree[(Sym, String)]]) =>
    rem match {
      case h :: Nil => h.successNel
      case h :: t => new Throwable("Ambiguous command. Too many commands match the input").failureNel
      case Nil => new Throwable("Input is not a valid command").failureNel
    }
  }

  def run[A](m: Store[MapT, A]) = step { (command: Tree[(Sym, String)]) =>
    val key = command map (_._1)
    m.get(key)
      .fold(new Throwable("Unknown command").failureNel[A]) { runner =>
        (normalise _ andThen runner.run) (command)
      }
  }
}

object Validators {
  //TODO: Should the normalisation be part of the reification process?
  def normalise(syntax: Tree[(Sym, String)]): List[String] = {
    syntax filterL (_._1 isTyped) match {
      case list@h :: t =>
        list.map {
          case ((Assign(l, op, _), input)) => (input split op) (1)
          case (_, input) => input
        }
      case Nil => List("")
    }
  }

  def syntax(assoc: (Sym, String)) = assoc._1 match {
    case Com(label, _) =>
      if (label == assoc._2) assoc.right
      else new Throwable(s"Input of `${assoc._2}` does not match expected `$label`").left
    case Named(label, _) =>
      if (label == assoc._2) assoc.right
      else new Throwable(s"Input of `${assoc._2}` does not match expected `$label`").left
    case Assign(label, op, _) =>
      if (assoc._2 startsWith (label + op)) assoc.right
      else new Throwable(s"Improper assignment for `$label`").left
    case Alt(th, tht, _) =>
      if (assoc._2 == th || assoc._2 == tht) assoc.right
      else new Throwable(s"None of the provided alternatives $th or $tht match input of ${assoc._2}").left
    case _ => assoc.right
  }

  def types(assoc: (Sym, String)) = assoc._1 match {
    case Type(m) =>
      Try(m(assoc._2)) match {
        case Success(_) => assoc.right
        case Failure(_) => new Throwable(s"Could not prove type of `${assoc._2}`").left
      }
    case _ => assoc.right
  }
}

