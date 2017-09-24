package buccaneer

import shapeless._
import ops.hlist._
import CommandBuilder._
import buccaneer.Read.readWhen
import scalaz.Reader

trait CommandOps {
  implicit def commandOps(command: Com): CommandBuilder[HNil, HNil, HNil, Unit] = emptyBuilder - command
  implicit def optionOps(option: Opt): CommandBuilder[HNil, HNil, HNil, Unit] = emptyBuilder - option
  implicit def argumentOps[A](argument: Arg[A]): CommandBuilder[Read[A] :: HNil, A :: HNil, A :: HNil, Tuple1[A]] = emptyBuilder - argument
  implicit def assignmentOps[A](assignment: Assgn[A]): CommandBuilder[Read[A] :: HNil, A :: HNil, A :: HNil, Tuple1[A]] = emptyBuilder - assignment


  /** Plucks a `Read[A]` instance out of implicit scope
    * and returns it
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
  def assignment[A: Read](labels: String*)(op: String): Assgn[A] = Assgn(labels.toList, op, prove[A], "")

  /**
    * Creates a constrained association between an identifier and a type.
    *
    * @param labels a sequence of possible names for the assignment
    * @param p predicate constraint on future value
    * @tparam A desired type
    * @return typed identifier denotation
    */
  def assignment[A: Read](p: A => Boolean)(labels: String*)(op: String): Assgn[A] = Assgn(labels.toList, op, proveWhen(p), "")
}

private[buccaneer]
object consume extends Poly2 {
  implicit def f[A, H <: HList]: Case.Aux[(Args, H), Read[A], (Args, A :: H)] = at[(Args, H), Read[A]] {
    case ((args, hlist), read) =>
      val a = read(args.head).toOption.get
      (args.tail, a :: hlist)
  }
}

object CommandBuilder {
  val emptyBuilder: CommandBuilder[HNil, HNil, HNil, Unit] =
    CommandBuilder(
      Tree.empty[Denotation[Any]],
      HNil,
      Reverse.reverse[HNil, HNil],
      Tupler.hnilTupler,
      LeftFolder.hnilLeftFolder[(Args, HNil), consume.type])
}

case class CommandBuilder[K <: HList, H <: HList, R <: HList, T](tree: Tree[Denotation[Any]],
                                                                 types: K,
                                                                 reverse: Reverse.Aux[H, R],
                                                                 tupler: Tupler.Aux[R, T],
                                                                 folder: LeftFolder.Aux[K, (Args, HNil), consume.type, (Args, R)]) {

  def -[A, B, O <: HList](argument: Arg[A])
                         (implicit
                          r: Reverse.Aux[A :: H, O],
                          t: Tupler.Aux[O, B],
                          f: LeftFolder.Aux[Read[A] :: K, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilder[Read[A] :: K, A :: H, O, B]
  = CommandBuilder(tree infix argument, argument.read :: types, r, t, f)

  def -[A, B, O <: HList](assignment: Assgn[A])
                         (implicit
                          r: Reverse.Aux[A :: H, O],
                          t: Tupler.Aux[O, B],
                          f: LeftFolder.Aux[Read[A] :: K, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilder[Read[A] :: K, A :: H, O, B]
  = CommandBuilder(tree infix assignment, assignment.read :: types, r, t, f)

  def -(option: Opt): CommandBuilder[K, H, R, T] = CommandBuilder(tree infix option, types, reverse, tupler, folder)
  def -(command: Com): CommandBuilder[K, H, R, T] = CommandBuilder(tree affix command, types, reverse, tupler, folder)

  def apply[B](f: T => B): Command[B] = Command(tree, Reader { args =>
    attempt(f(tupler(folder(types, (args.reverse, HNil))._2)))
  })
}

case class Command[A](expr: Tree[Denotation[Any]], fn: Reader[Args, Result[A]])