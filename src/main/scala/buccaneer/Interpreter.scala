package buccaneer

import buccaneer.Cli.Cli

import scalaz.syntax.validation._
import scalaz.{Failure, Kleisli}
import buccaneer.Validators._

/** An ADT for modelling the different types of steps in the interpreter.
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
    * interpretation. Automatically prints errors and
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

  def phase[A, B](f: A => Step[B]): Phase[A, B] = Kleisli(f)

  def transform[A, B](f: A => Result[B]): Phase[A, B] = phase(f andThen Transform.apply)

  /** Partially matches a command input to a set of given command Exprs and
    * keeps those that match most closely. The matching occurs from left-to-right.
    *
    * @param commands Exprs of commands to match against
    * @param input    input of command elements
    * @return set of command Exprs that partially match the input
    */
  // TODO: This has to be exact in the input length. `zip` on vector will not exclude valid partial expressions
  def partialMatch(commands: Set[Expr[Any]], input: Args): Set[Expr[Any]] =
    if (input.isEmpty) commands
    else commands.filter {
      _.zip(input).
        forall {
          case (Com(label, _), value) => label startsWith value
          case (Opt(labels, _), value) => labels exists (label => label startsWith value)
          case (Arg(read, _), value) => read(value).isSuccess
          case (Assgn(labels, _, _, _), value) => labels exists (label => (label startsWith value) || (value startsWith label))
        }
    }

  /** Partially matches an input against a set of command Exprs and
    * automatically returns a failure if no commands partially match.
    *
    * @param commands command Exprs to match against
    * @param input input of command elements
    * @return an interpretation step
    */
  def matching(commands: Set[Expr[Any]], input: Args): Step[Set[Expr[Any]]] =
    partialMatch(commands, input) match {
      case set if set.isEmpty => Transform(failure("No command found matching input"))
      case set => Transform(success(set))
    }

  /** Interprets a single command.
    * In case of erroneous input, all detected errors are accumulated in a list.
    *
    * @param command to interpret
    * @tparam A type of command result
    * @return an interpretation step
    */
  def interpret[A](command: Command[A]) = resolve(command.expr.expand) andThen run(command)


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
    * By default, for man pages, any command should end in `-help` or `--help`.
    * By default, for suggestions, any command should end in `-sgst` or `--sgst`.
    * These options can however be changed in `ManConfig`.
    *
    * @param cli       the command line interface
    * @param manConfig the configuration record for MAN pages
    * @tparam A type of the command result
    * @return an interpretation step
    */
  def interpretH[A](cli: Cli[A], manConfig: ManConfig = ManConfig.man()) =
    meta(cli, manConfig) andThen resolve(cli.keySet) andThen runFrom(cli)

  /** Provides MAN page generation and suggestions for a command line interface.
    *
    * @param cli       the command line interface
    * @param manConfig the configuration record for MAN pages
    * @tparam A type of the command result
    * @return an interpretation step
    */
  def meta[A](cli: Cli[A], manConfig: ManConfig) = phase { (input: List[String]) =>
    (for {
      opt <- input.lastOption
      help = manConfig.help.labels.find(_ == opt).map(_ => Man.help(_))
      suggest = manConfig.suggest.labels.find(_ == opt).map(_ => Man.suggest(_))
      helpOrSuggest <- help orElse suggest
      command = input.dropRight(1)
    } yield matching(cli.keySet, command).flatMap { matching =>
      Meta(helpOrSuggest(matching.map(x => traverseList.zipL(x, command))).run(manConfig))
    }).
      getOrElse(Transform(success(input)))
  }
  /** Picks out the appropriate command from a set of command Exprs when
    * given a command line input.
    *
    * Returns a failed interpretation if no command found.
    *
    * @param all set of command Exprs
    * @return an interpretation step
    */
  def resolve(all: Set[Expr[Any]]) =
    interpolate(all) andThen validate andThen pick

  /** Zips a command line input with all command Exprs that matches that input in size.
    *
    * @param commands command Exprs
    * @return an interpretation step
    */
  def interpolate(commands: Set[Expr[Any]]) = transform { (input: List[String]) =>
    val args = if (input.isEmpty) List("") else input
    commands.
      filter(_.size == args.size).
      map(x => traverseList.zipL(x, args)).
      successNel
  }

  /** Strictly validates the syntax and types of an interpolated AST.
    *
    * @return List of AST's that pass the validation
    */
  def validate = transform { candidates: Set[AST[Any]] =>
    candidates.
      filter { candidate =>
        Validators.validate(candidate)(syntax).isSuccess &&
        Validators.validate(candidate)(types).isSuccess
      }.
      successNel
  }

  /** Picks the final command Expr from a list of possible Exprs.
    * It is considered to be last step in the command resolution process.
    * If, after command resolution, the list of possible Exprs contains
    * more than one command, then the input is ambiguous. If it however contains none,
    * then no command Expr has been found that exactly matches that input.
    *
    * @return an interpretation step
    */
  def pick = transform { (commands: Set[AST[Any]]) =>
    commands match {
      case set if set.isEmpty => failure("No command found matching input")
      case set if set.size == 1 => success(set.head)
      case set => failure {
        s"Ambiguous input. There are ${set.size} matches for this input\n${
          set.
            map(_.map(_._1.show).mkString(" ")).
            map(x => Formatter(x).push(5).runMake).
            mkString("")}"
      }
    }
  }

  /** Runs the function associated with a command Expr from a command
    * line interface.
    *
    * @param cli command line interface
    * @tparam A type of the command result
    * @return an interpretation step
    */
  def runFrom[A](cli: Cli[A]) = phase { (ast: AST[Any]) =>
    val expr = ast map (_._1)

    cli.get(expr).
      map(cmd => run(cmd).run(ast)).
      getOrElse(Transform(failure("Unknown command")))
  }

  /** Runs the function of a single command.
    *
    * @param command command to run
    * @tparam A type of command result
    * @return an interpretation step
    */
  def run[A](command: Command[A]): Phase[AST[Any], A] = transform { (ast: AST[Any]) =>
    val args = ast.filter(_._1.isTyped).map(_._2.get)
    command.fn(args)
  }
}

object Validators {

  def validate[A](v: List[A])(f: A => Result[A]): Result[List[A]] = traverseList.traverse(v)(f)

  /** Function for validating the syntax of an input against its expectation.
    *
    * @param value association between expectation and concrete input
    * @return result of validation
    */
  def syntax(value: (Denotation[Any], Option[String])): Result[(Denotation[Any], Option[String])] = value match {
    case (Com(label, _), Some(input)) if label == input => success(value)
    case (Opt(labels, _), Some(input)) if labels contains input => success(value)
    case (Assgn(labels, op, _, _), Some(input)) if labels exists (label => input startsWith (label + op)) => success(value)
    case (Arg(_, _), _) => success(value)
    case _ => failure(s"Input of `${value._2}` does not match the expected input of ${value._1.show}")
  }

  /** Function for validation the type of an input against its expectation.
    *
    * @param value association between expectation and concrete input
    * @return result of validation
    */
  def types(value: (Denotation[Any], Option[String])): Result[(Denotation[Any], Option[String])] = value match {
    case (Arg(read, _), Some(input)) => read(input).map(_ => value)
    case (Assgn(_, _, read, _), Some(input)) => read(input).map(_ => value)
    case (Arg(read, _), None) => failure(s"Cannot coerce empty input to type ${read.show}")
    case (Assgn(_, _, read, _), None) => failure(s"Cannot coerce empty input to type ${read.show}")
    case _ => success(value)
  }
}