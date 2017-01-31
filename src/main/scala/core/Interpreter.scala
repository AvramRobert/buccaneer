package core

import core.Binary.treeSyntax
import core.Man.{HelpConfig, Section}
import core.Read.Result
import core.Store._
import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scalaz.{Failure, Kleisli}
import core.Validators._

object Interpreter {
  sealed trait Step[+A] {
    def flatMap[B](f: A => Step[B]): Step[B] = this match {
      case Transform(result) => (result map f).fold(
        x => Transform(Failure(x)),
        identity)
      case Meta(info) => Meta(info)
    }

    def map[B](f: A => B): Step[B] = this match {
      case Transform(result) => Transform(result map f)
      case Meta(info) => Meta(info)
    }

    def fold[B](success: A => B)
               (fail: List[Throwable] => B)
               (meta: String => B): B = this match {
      case Transform(result) => result.fold(errs => fail(errs.list), success)
      case Meta(info) => meta(info)
    }
  }

  case class Transform[A](result: Result[A]) extends Step[A]

  case class Meta(info: String) extends Step[Nothing]

  type Phase[A, B] = Kleisli[Step, A, B]
  type AST = Tree[(Denot, String)]
  type Shape = Tree[Denot]

  def partialMatch(commands: Set[Shape], input: List[String]): Set[Shape] =
    commands.filter {
      _.zipL(input).
        serialise.
        forall {
          case (Identifier(sym, _, _), value) => sym.isSymbol(value)
          case (TypedIdentifier(sym, _, _), value) => sym.find(a => value startsWith a).fold(false)(_ => true)
          case (Typing(p, _), value) => p(value).isSuccess
        }
    }

  def phase[A, B](f: A => Step[B]): Phase[A, B] = Kleisli(f)

  def transform[A, B](f: A => Result[B]): Phase[A, B] = phase(f andThen Transform.apply)

  def interpret[A](cmd: Cmd[A]) =
    interpolate(Set(cmd.syntax)) andThen
      pick andThen
      transform { (ast: AST) =>
        (ast.validate(syntax) |@| ast.validate(types)) ((_, t) => t)
      } andThen
      run(cmd)

  def interpret[A](store: Store[MapT, A]) = resolve(store.keySet) andThen runFrom(store)

  def interpretH[A](store: Store[MapT, A], helpConfig: HelpConfig = HelpConfig(150, 5, 5)) =
    meta(store, helpConfig) andThen resolve(store.keySet) andThen runFrom(store)

  def resolve(all: Set[Shape]) =
    interpolate(all) andThen
      filter(_.rootOf {
        case (Identifier(Label(value), _, _), input) => input == value
        case _ => false
      }) andThen
      validate(syntax) andThen
      validate(types) andThen
      pick


  def meta[A](store: Store[MapT, A], helpConfig: HelpConfig) = phase { (input: List[String]) =>
    def show(f: (List[String], Set[Shape]) => Section[String]): Step[Nothing] = {
      lazy val command = input.dropRight(1)
      partialMatch(store.keySet, command) match {
        case set if set.isEmpty => Transform(new Throwable("Unknown command").failureNel)
        case set => Meta(f(command, set).run(helpConfig))
      }
    }

    input.last match {
      case "-help" | "--help" => show(Man.help)
      case "-sgst" | "--sgst" => show(Man.suggest)
      case _ => Transform(input.successNel)
    }
  }

  def interpolate(commands: Set[Shape]) = transform { (input: List[String]) =>
    commands.
      filter(_.depth == input.size).
      map(_ zipL input).
      toList.
      successNel
  }

  def filter(f: AST => Boolean) = transform { (commands: List[AST]) =>
    commands.filter(f).successNel
  }

  def validate(f: ((Denot, String)) => Result[(Denot, String)]) = transform { (commands: List[AST]) =>
    val point = List.empty[AST].successNel[Throwable]
    commands.map(_ validate f)
      .filter(_.isSuccess)
      .foldRight(point)((a, b) => (a |@| b) (_ :: _))
  }

  def pick = transform { (commands: List[AST]) =>
    commands match {
      case h :: Nil => h.successNel
      case Nil => new Throwable("No command found matching input").failureNel
      case _ => new Throwable(s"Ambiguous input. ${commands.size} match given input").failureNel
    }
  }

  def runFrom[A](store: Store[MapT, A]) = phase { (command: AST) =>
    val key = command map (_._1)
    val fail = Transform(new Throwable("Unknown command").failureNel[A])

    store.get(key).fold[Step[A]](fail)(cmd => run(cmd).run(command))
  }

  def run[A](cmd: Cmd[A]): Phase[AST, A] = transform { (ast: AST) =>
    val args = ast.filterL(_._1.isTyped).map(_._2)
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
