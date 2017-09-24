package buccaneer

import shapeless._
import ops.hlist._
import SCommand._
import Denotation._
import scalaz.Reader
import Implicits._

object consume extends Poly2 {
  implicit def f[A, H <: HList] = at[(Args, H), Read[A]] {
    case ((args, hlist), read) =>
      val a = read(args.head).toOption.get
      (args.tail, a :: hlist)
  }
}

object SCommand {
  type Args = List[String]

  val emptyBuilder: CommandBuilder[HNil, HNil, HNil, Unit] =
    CommandBuilder(
      HNil,
      Reverse.reverse[HNil, HNil],
      Tupler.hnilTupler,
      LeftFolder.hnilLeftFolder[(Args, HNil), consume.type])

  implicit def commandOps(c: Com): CommandBuilder[HNil, HNil, HNil, Unit] = emptyBuilder
  implicit def optionOps(o: Opt): CommandBuilder[HNil, HNil, HNil, Unit] = emptyBuilder
  implicit def argumentOps[A](argument: Arg[A]): CommandBuilder[Read[A] :: HNil, A :: HNil, A :: HNil, Tuple1[A]] = emptyBuilder - argument
  implicit def assignmentOps[A](assignment: Assgn[A]): CommandBuilder[Read[A] :: HNil, A :: HNil, A :: HNil, Tuple1[A]] = emptyBuilder - assignment
}

case class CommandBuilder[K <: HList, H <: HList, R <: HList, T](types: K,
                                                                 reverse: Reverse.Aux[H, R],
                                                                 tupler: Tupler.Aux[R, T],
                                                                 folder: LeftFolder.Aux[K, (Args, HNil), consume.type, (Args, R)]) {

  def -[A, B, O <: HList](argument: Arg[A])
                         (implicit
                          r: Reverse.Aux[A :: H, O],
                          t: Tupler.Aux[O, B],
                          f: LeftFolder.Aux[Read[A] :: K, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilder[Read[A] :: K, A :: H, O, B]
  = CommandBuilder(argument.read :: types, r, t, f)

  def -[A, B, O <: HList](assignment: Assgn[A])
                         (implicit
                          r: Reverse.Aux[A :: H, O],
                          t: Tupler.Aux[O, B],
                          f: LeftFolder.Aux[Read[A] :: K, (Args, HNil), consume.type, (Args, O)])
  : CommandBuilder[Read[A] :: K, A :: H, O, B]
  = CommandBuilder(assignment.read :: types, r, t, f)

  def -(command: Com): CommandBuilder[K, H, R, T] = CommandBuilder(types, reverse, tupler, folder)
  def -(option: Opt): CommandBuilder[K, H, R, T] = CommandBuilder(types, reverse, tupler, folder)

  def apply[B](f: T => B): Reader[Args, B] = Reader { args =>
    f(tupler(folder(types, (args.reverse, HNil))._2))
  }
}

object quick {
  import SCommand._

  def main(args: Array[String]): Unit = {
    val c =
      (argument[Int] - argument[Boolean] - argument[List[Int]]).apply {
        case (i, b, lst) => s"${i.toString} and $b and ${lst.mkString(" : ")}"
      }

    println(c.run(List("1", "true", "1,2,3,4")))
  }
}

