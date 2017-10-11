package buccaneer

import shapeless._
import ops.hlist._
import buccaneer.Read.readWhen
import RoseList._

trait CommandOps {

  /** Defines a builder context for commands
    *
    */
  val >> : CommandBuilder[HNil, HNil, HNil, Unit, Unit] = CommandBuilder.emptyBuilder

  /** Plucks a `Read[A]` instance out of implicit scope
    * and returns it
    *
    * @tparam A type of `Read`
    * @return `Read[A]` instance
    */
  private def prove[A: Read]: Read[A] = implicitly[Read[A]]

  /** Picks a `Read` instance from implicit scope and
    * transforms it into a constrained `Read`
    *
    * @param p predicate constraint on the future value
    * @tparam A type of `Read`
    * @return constrained `Read` instance
    */
  private def proveWhen[A: Read](p: A => Boolean): Read[A] = readWhen(prove)(p)

  /** Creates a command identifier.
    *
    * @param label command name as symbol
    * @return identifier denotation
    */
  def command(label: String): Com = Com(label, "")

  /** Creates an option identifier.
    *
    * @param labels a sequence of possible options
    * @return identifier denotation
    */
  def option(labels: String*): Opt = Opt(labels.toList, "")

  /** Creates a type argument.
    *
    * @tparam A desired type
    * @return typing denotation
    */
  def argument[A: Read]: Arg[A] = Arg(prove[A], "")

  /**
    * Creates a constrained type argument.
    *
    * @param p predicate constraint on the future value
    * @tparam A desired type
    * @return typing denotation
    */
  def argument[A: Read](p: (A) => Boolean): Arg[A] = Arg(proveWhen(p), "")

  /** Creates an association between an identifier and a type.
    *
    * @param labels a sequence of possible names for the assignment
    * @tparam A desired type
    * @return typed identifier denotation
    */
  def assignment[A: Read](labels: String*)(op: String): Assgn[A] = {
    val read = Read.constrain(prove[A])(_.split(op)(1))
    Assgn(labels.toList, op, read, "")
  }

  /**
    * Creates a constrained association between an identifier and a type.
    *
    * @param labels a sequence of possible names for the assignment
    * @param p      predicate constraint on future value
    * @tparam A desired type
    * @return typed identifier denotation
    */
  def assignment[A: Read](p: A => Boolean)(labels: String*)(op: String): Assgn[A] = {
    val read = Read.constrain(prove[A])(_.split(op)(1))
    Assgn(labels.toList, op, readWhen(read)(p), "")
  }
}

private[buccaneer]
object consume extends Poly2 {
  implicit def f[A, H <: HList]: Case.Aux[(Args, H), Read[A], (Args, A :: H)] = at[(Args, H), Read[A]] {
    case ((args, hlist), read) =>
      val a = read(args.head).toOption.get
      (args.tail, a :: hlist)
  }
}

final case class Command[+A](expr: RoseList[Denotation[Any]], fn: Args => Result[A])

object CommandBuilder {
  val emptyBuilder: CommandBuilder[HNil, HNil, HNil, Unit, Unit] =
    CommandBuilder(
      empty,
      HNil,
      Reverse.reverse[HNil, HNil],
      Tupler.hnilTupler,
      Fixer.id,
      LeftFolder.hnilLeftFolder)
}

private[buccaneer]
final case class CommandBuilder[K <: HList, H <: HList, R <: HList, T, X]
(expr: RoseList[Denotation[Any]],
 types: K,
 reverse: Reverse.Aux[H, R],
 tupler: Tupler.Aux[R, T],
 fixer: Fixer.Aux[T, X],
 folder: LeftFolder.Aux[K, (Args, HNil), consume.type, (Args, R)]) {

  def -[A, B, C, O <: HList](argument: Arg[A])
                            (implicit
                             r: Reverse.Aux[A :: H, O],
                             t: Tupler.Aux[O, B],
                             x: Fixer.Aux[B, C],
                             f: LeftFolder.Aux[Read[A] :: K, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilder[Read[A] :: K, A :: H, O, B, C]
  = CommandBuilder(expr prepend argument, argument.read :: types, r, t, x, f)

  def -[A, B, C, O <: HList](assignment: Assgn[A])
                            (implicit
                             r: Reverse.Aux[A :: H, O],
                             t: Tupler.Aux[O, B],
                             x: Fixer.Aux[B, C],
                             f: LeftFolder.Aux[Read[A] :: K, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilder[Read[A] :: K, A :: H, O, B, C]
  = CommandBuilder(expr prepend assignment, assignment.read :: types, r, t, x, f)

  def -(command: Com): CommandBuilder[K, H, R, T, X] = CommandBuilder(expr prepend command, types, reverse, tupler, fixer, folder)

  def -(options: Opt*): CommandBuilder[K, H, R, T, X] = CommandBuilder(expr prepend (options: _*), types, reverse, tupler, fixer, folder)

  def apply[B](f: X => B): Command[B] = Command(expr, args =>
    attempt(f(fixer(tupler(folder(types, (args.reverse, HNil))._2)))))
}