package core

import core.Man.Section
import core.Read.Result
import core.Cli.Cli
import core.Read._

import scalaz.syntax.applicative._
import scalaz.syntax.validation._
import scalaz.{Failure, Kleisli}
import core.Validators._

/** A GADT for modelling the different types of steps in the interpreter.
  *
  * @tparam A type of interpretation
  */
sealed trait Step[+A] {
  /** Binds one interpretation step to another step.
    * Binding occurs only in the case of a successful transformation.
    *
    * @param f binding function
    * @tparam B type of binding
    * @return a new step containing the bind result
    */
  def flatMap[B](f: A => Step[B]): Step[B] = this match {
    case Transform(result) => (result map f).fold(
      x => Transform(Failure(x)),
      identity)
    case Meta(info) => Meta(info)
  }

  /** Applies a function on the value of an interpretation.
    * Only applies it in case of a successful transformation.
    *
    * @param f morphism
    * @tparam B type of morphism
    * @return a new step containing the morphed value
    */
  def map[B](f: A => B): Step[B] = this match {
    case Transform(result) => Transform(result map f)
    case Meta(info) => Meta(info)
  }

  /** Folds over the possible cases of an interpretation
    *
    * @param success function for the success case
    * @param fail    function for the failure case
    * @param meta    function for the meta case
    * @tparam B type of resulting structure
    * @return some value
    */
  def fold[B](success: A => B)
             (fail: List[Throwable] => B)
             (meta: String => B): B = this match {
    case Transform(result) => result.fold(errs => fail(errs.list.toList), success)
    case Meta(info) => meta(info)
  }

  /** Prints the results of an interpretation.
    * It uses a pre-defined formatting for errors.
    *
    */
  def print: Unit = fold(println)(errs => formatErrors(errs) foreach println)(println)

  /** Applies a side-effecting function on the success value of an
    * iterpretation. Automatically prints errors and
    * meta information with a predefined format.
    *
    * @param f side-effecting function
    */
  def foreach(f: A => Unit): Unit = fold(f)(errs => formatErrors(errs) foreach println)(println)

  /** Minimalistic formatting for errors.
    *
    * @param errors the errors to be formatted
    * @return a list of formatted strings
    */
  def formatErrors(errors: List[Throwable]): List[String] = {
    (Formatter(s"Command failed (${errors.size} errors):") ::
      errors.
        zipWithIndex.
        map(t => Formatter(s"${t._2 + 1}. ${t._1.toString}").push(2))).
      map(_.runMake)
  }
}

/** Represents the outcome of a transformation step of the interpreter.
  * A transformation can either succeed or fail.
  *
  * @param result result of transformation
  * @tparam A type of interpretation
  */
case class Transform[A](result: Result[A]) extends Step[A]

/** Represents the outcome of an interpretation, where the
  * emission of some meta information has been triggered.
  *
  * @param info emitted information
  */
case class Meta(info: String) extends Step[Nothing]

object Interpreter {
  type Phase[A, B] = Kleisli[Step, A, B]
  type AST = Tree[(Denot, String)]
  type Shape = Tree[Denot]

  /** Partially matches a command input to a set of given command shapes and
    * keeps those that match most closely. The matching occurs from left-to-right.
    *
    * @param commands shapes of commands to match against
    * @param input    input of command elements
    * @return set of command shapes that partially match the input
    */
  def partialMatch(commands: Set[Shape], input: List[String]): Set[Shape] =
    if (input.isEmpty) commands
    else commands.filter {
      _.zipWithList(input).
        forall {
          case (Identifier(sym, _, _), value) => sym.exists(_ startsWith value)
          case (TypedIdentifier(sym, _, _), value) => sym.exists(x => (x startsWith value) || (value startsWith x))
          case (Typing(p, _), value) => p(value).isSuccess
        }
    }

  def phase[A, B](f: A => Step[B]): Phase[A, B] = Kleisli(f)

  def transform[A, B](f: A => Result[B]): Phase[A, B] = phase(f andThen Transform.apply)

  /** Interprets a single command.
    * In case of erroneous input, all detected errors are accumulated in a list.
    *
    * @param cmd command to interpret
    * @tparam A type of command result
    * @return an interpretation step
    */
  def interpret[A](cmd: Cmd[A]) =
    interpolate(Set(cmd.syntax)) andThen
      pick andThen
      transform { (ast: AST) =>
        (ast.validate(syntax) |@| ast.validate(types)) ((_, t) => t)
      } andThen
      run(cmd)

  /** Provides interpretation for an entire command line interface.
    * When run, it automatically picks, checks and runs the appropriate command
    * given some command line input. Does NOT provide MAN page generation and
    * suggestions. (see `interpretH` for that)
    *
    * @param cli the command line interface
    * @tparam A type of the command result
    * @return an interpretation step
    */
  def interpret[A](cli: Cli[A]) = resolve(cli.keySet) andThen runFrom(cli)

  /** Provides interpretation for an entire command line interface.
    * When run, it automatically picks, checks and runs the appropriate command
    * given some command line input. It additionally supports MAN page generation
    * and suggestions at any point during the invocation.
    *
    * For man pages, any command should end in `-help` or `--help`.
    * For suggestions, any command should end in `-sgst` or `--sgst`.
    *
    * @param cli       the command line interface
    * @param manConfig the configuration record for MAN pages
    * @tparam A type of the command result
    * @return an interpretation step
    */
  def interpretH[A](cli: Cli[A], manConfig: ManConfig = ManConfig.man()) =
    meta(cli, manConfig) andThen resolve(cli.keySet) andThen runFrom(cli)

  /** Picks-out the appropriate command a set of command shapes when
    * given a command line input.
    *
    * @param all set of command shapes
    * @return an interpretation step
    */
  def resolve(all: Set[Shape]) =
    interpolate(all) andThen
      filter(_.rootOf[(Denot, String)] {
        case (Identifier(Label(value), _, _), input) => input == value
        case _ => true
      }) andThen
      validate(syntax) andThen
      validate(types) andThen
      pick

  /** Provides MAN page generation and suggestions for a command line interface.
    *
    * @param cli       the command line interface
    * @param manConfig the configuration record for MAN pages
    * @tparam A type of the command result
    * @return an interpretation step
    */
  def meta[A](cli: Cli[A], manConfig: ManConfig) = phase { (input: List[String]) =>
    def show(f: (List[String], Set[Shape]) => Section[String]): Step[Nothing] = {
      lazy val command = input.dropRight(1)
      partialMatch(cli.keySet, command) match {
        case set if set.isEmpty => Transform(failure("No command found matching input"))
        case set => Meta(f(command, set).run(manConfig))
      }
    }

    (manConfig.help.symbol.find(_ == input.last),
      manConfig.suggest.symbol.find(_ == input.last)) match {
      case (Some(_), _) => show(Man.help)
      case (_, Some(_)) => show(Man.suggest)
      case _ => Transform(success(input))
    }
  }

  /** Zips a command line input with all command shapes that matches that input in size.
    *
    * @param commands command shapes
    * @return an interpretation step
    */
  def interpolate(commands: Set[Shape]) = transform { (input: List[String]) =>
    commands.
      filter(_.depth == input.size).
      map(_ zipWithList input).
      toList.
      successNel
  }

  /** Keeps interpolated command line shapes that satisfy the given predicate.
    *
    * @param f predicate stating the condition of preservation
    * @return an interpretation step
    */
  def filter(f: AST => Boolean) = transform { (commands: List[AST]) =>
    success(commands.filter(f))
  }

  /** Applies a validation function on a list of interpolated command
    * line shapes. Essentially used to validate the concrete input against
    * the expectation with which it has been zipped.
    *
    * @param f validation function
    * @return an interpretation step
    */
  def validate(f: ((Denot, String)) => Result[(Denot, String)]) = transform { (commands: List[AST]) =>
    val point = success(List.empty[AST])
    commands.map(_ validate f)
      .filter(_.isSuccess)
      .foldRight(point)((a, b) => (a |@| b) (_ :: _))
  }

  /** Picks the final command shape from a list of possible shapes.
    * It is considered to be last step in the command resolution process.
    * If, after command resolution, the list of possible shapes contains
    * more than one command, then the input is ambiguous. If it however contains none,
    * then no command shape has been found that exactly matches that input.
    *
    * @return an interpretation step
    */
  def pick = transform { (commands: List[AST]) =>
    commands match {
      case h :: Nil => success(h)
      case Nil => failure("No command found matching input")
      case _ => failure(s"Ambiguous input. ${commands.size} match given input")
    }
  }

  /** Runs the function associated with a command shape from a command
    * line interface.
    *
    * @param cli command line interface
    * @tparam A type of the command result
    * @return an interpretation step
    */
  def runFrom[A](cli: Cli[A]) = phase { (command: AST) =>
    val key = command map (_._1)
    val fail = Transform(failure("Unknown command"))

    cli.get(key).fold[Step[A]](fail)(cmd => run(cmd).run(command))
  }

  /** Runs the function of a single command.
    *
    * @param cmd command to run
    * @tparam A type of command result
    * @return an interpretation step
    */
  def run[A](cmd: Cmd[A]): Phase[AST, A] = transform { (ast: AST) =>
    val args = ast.filter(_._1.isTyped).map(_._2)
    cmd.run(args)
  }
}

object Validators {

  /** Function for validating the syntax of an input against its expectation.
    *
    * @param assoc association between expectation and concrete input
    * @return result of validation
    */
  def syntax(assoc: (Denot, String)) = (assoc match {
    case (Identifier(symbol, _, _), input) => symbol.find(_ == input)
    case (TypedIdentifier(symbol, _, _), input) => symbol.find(v => input startsWith v)
    case _ => Some(Label(""))
  }).fold {
    failure[(Denot, String)](s"Input of `${assoc._2}` does not match the expected input of ${assoc._1.show}")
  } { _ => success(assoc) }

  /** Function for validation the type of an input against its expectation.
    *
    * @param assoc association between expectation and concrete input
    * @return result of validation
    */
  def types(assoc: (Denot, String)) = assoc match {
    case (Typing(proof, _), input) => proof(input).map(_ => assoc)
    case (TypedIdentifier(_, proof, _), input) => proof(input).map(_ => assoc)
    case _ => success(assoc)
  }
}
