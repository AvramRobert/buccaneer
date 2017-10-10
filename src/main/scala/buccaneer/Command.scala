package buccaneer

import shapeless._
import ops.hlist._
import CommandBuilder._
import buccaneer.Read.readWhen
import RoseList._

trait CommandOps {
  implicit def commandOps(command: Com): CommandBuilder0 = emptyBuilder - command

  implicit def optionOps(option: Opt): CommandBuilder0 = emptyBuilder - option

  implicit def argumentOps[A](argument: Arg[A]): CommandBuilder1[A] = emptyBuilder - argument

  implicit def assignmentOps[A](assignment: Assgn[A]): CommandBuilder1[A] = emptyBuilder - assignment


  /** Plucks a `Read[A]` instance out of implicit scope
    * and returns it
    *
    * @tparam A type of `Read`
    * @return `Read[A]` instance
    */
  def prove[A: Read]: Read[A] = implicitly[Read[A]]

  /** Picks a `Read` instance from implicit scope and
    * transforms it into a constrained `Read`
    *
    * @param p predicate constraint on the future value
    * @tparam A type of `Read`
    * @return constrained `Read` instance
    */
  def proveWhen[A: Read](p: A => Boolean): Read[A] = readWhen(prove)(p)

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

final case class Command[A](expr: RoseList[Denotation[Any]], fn: Args => Result[A])

object CommandBuilder {
  val emptyBuilder: CommandBuilder0 = CommandBuilder0(empty[Denotation[Any]])
}

private[buccaneer]
final case class CommandBuilder0(expr: RoseList[Denotation[Any]]) {
  def -[A](argument: Arg[A]): CommandBuilder1[A] = CommandBuilder1(expr prepend argument, argument.read :: HNil)

  def -[A](assignment: Assgn[A]): CommandBuilder1[A] = CommandBuilder1(expr prepend assignment, assignment.read :: HNil)

  def -(option: Opt): CommandBuilder0 = CommandBuilder0(expr prepend option)

  def -(command: Com): CommandBuilder0 = CommandBuilder0(expr prepend command)

  def apply[B](f: Unit => B): Command[B] = Command(expr, _ => attempt(f()))
}

private[buccaneer]
final case class CommandBuilder1[T](expr: RoseList[Denotation[Any]],
                                    types: Read[T] :: HNil) {

  def -[A, B, O <: HList](argument: Arg[A])
                         (implicit
                          r: Reverse.Aux[A :: T :: HNil, O],
                          t: Tupler.Aux[O, B],
                          f: LeftFolder.Aux[Read[A] :: Read[T] :: HNil, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilderN[Read[A] :: Read[T] :: HNil, A :: T :: HNil, O, B]
  = CommandBuilderN(expr prepend argument, argument.read :: types, r, t, f)

  def -[A, B, O <: HList](assignment: Assgn[A])
                         (implicit
                          r: Reverse.Aux[A :: T :: HNil, O],
                          t: Tupler.Aux[O, B],
                          f: LeftFolder.Aux[Read[A] :: Read[T] :: HNil, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilderN[Read[A] :: Read[T] :: HNil, A :: T :: HNil, O, B]
  = CommandBuilderN(expr prepend assignment, assignment.read :: types, r, t, f)

  def -(option: Opt): CommandBuilder1[T] = CommandBuilder1(expr prepend option, types)

  def -(command: Com): CommandBuilder1[T] = CommandBuilder1(expr prepend command, types)

  def apply[B](f: T => B): Command[B] = Command(expr, args =>
    attempt(f(consume.f((args, HNil) :: types)._2.head)))
}

private[buccaneer]
final case class CommandBuilderN[K <: HList, H <: HList, R <: HList, T](expr: RoseList[Denotation[Any]],
                                                                        types: K,
                                                                        reverse: Reverse.Aux[H, R],
                                                                        tupler: Tupler.Aux[R, T],
                                                                        folder: LeftFolder.Aux[K, (Args, HNil), consume.type, (Args, R)]) {

  def -[A, B, O <: HList](argument: Arg[A])
                         (implicit
                          r: Reverse.Aux[A :: H, O],
                          t: Tupler.Aux[O, B],
                          f: LeftFolder.Aux[Read[A] :: K, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilderN[Read[A] :: K, A :: H, O, B]
  = CommandBuilderN(expr prepend argument, argument.read :: types, r, t, f)

  def -[A, B, O <: HList](assignment: Assgn[A])
                         (implicit
                          r: Reverse.Aux[A :: H, O],
                          t: Tupler.Aux[O, B],
                          f: LeftFolder.Aux[Read[A] :: K, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilderN[Read[A] :: K, A :: H, O, B]
  = CommandBuilderN(expr prepend assignment, assignment.read :: types, r, t, f)

  def -(option: Opt): CommandBuilderN[K, H, R, T] = CommandBuilderN(expr prepend option, types, reverse, tupler, folder)

  def -(command: Com): CommandBuilderN[K, H, R, T] = CommandBuilderN(expr prepend command, types, reverse, tupler, folder)

  def apply[B](f: T => B): Command[B] = Command(expr, args =>
    attempt(f(tupler(folder(types, (args.reverse, HNil))._2))))
}