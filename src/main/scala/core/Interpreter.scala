package core

import scala.util.{Failure, Success, Try}
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scalaz.{Bind, Kleisli, ValidationNel, \/}
import scalaz.syntax.either._
import Validators._
import core.Store.MapT
import Store._

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

  def interpret[A](runner: Runner[A]): Step[List[String], A] = interpret(Store.empty + runner)

  def resolve(keySet: Set[Inter[Sym]]) =
    shape(keySet) andThen
      validate(syntax) andThen
      validate(types) andThen
      narrow

  def shape(commands: Set[Inter[Sym]]) = step { (input: List[String]) =>
    input match {
      case l@h :: _ =>
        commands
          .filter {
            case Com(a, _) :~ t => a == h
            case _ => false
          }
          .filter(_.depth == l.size)
          .map(_ zipL l)
          .toList
          .successNel
      case Nil => Nil.successNel
    }
  }

  def validate(f: ((Sym, String)) => Throwable \/ (Sym, String)) = step { (is: List[Inter[(Sym, String)]]) =>
    lazy val point = List.empty[Inter[(Sym, String)]].successNel[Throwable]
    is
      .map(Inter.validate(_)(f))
      .filter(_.isSuccess)
      .foldRight(point)((a, b) => (a |@| b) (_ :: _))
  }

  def narrow = step { (rem: List[Inter[(Sym, String)]]) =>
    rem match {
      case h :: Nil => h.successNel
      case h :: t => new Throwable("Ambiguous command. Too many commands match the input").failureNel
      case Nil => new Throwable("Input is not a valid command").failureNel
    }
  }

  def run[A](m: Store[MapT, A]) = step { (command: Inter[(Sym, String)]) =>
    val key = command map (_._1)
    m.get(key)
      .fold(new Throwable("Unknown command").failureNel[A]) { runner =>
        (normalise _ andThen runner.run) (command)
      }
  }
}

object Validators {
  def normalise(syntax: Inter[(Sym, String)]): List[String] = {
    syntax filterL (_._1 isTyped) map {
      case ((Assign(l, op, _), input)) => (input split op) (1)
      case (_, input) => input
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

