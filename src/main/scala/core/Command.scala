package core

import core.Command._
import scalaz.syntax.validation._
import scala.util.{Failure, Success, Try}
import scalaz.{State, _}

object Command {
  type Result[A] = ValidationNel[Throwable, A]
  type TypedParams[A] = IndexedState[List[String], List[String], Result[A]]

  def reify[A](f: Reified[A]): TypedParams[A] =
    for {
      params <- State.get[List[String]]
      _ <- State.modify[List[String]](_.tail)
    } yield {
      Try(f(params.head)) match {
        case Success(t) => t.successNel
        case Failure(e) => e.failureNel
      }
    }

  def apply(label: String): P0 = P0(Inter.lift(Com(label)))

  implicit val tryB = new Bind[Try] {
    override def bind[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa flatMap f

    override def map[A, B](fa: Try[A])(f: (A) => B): Try[B] = fa map f
  }

  // Caution: Doing the computation within `fa` reverses the order of the parameter application when running the Monad
  // I.e given a command.param[Int].param[String] <- List("1", "a"), it will feed `1` and `a` in reverse => first "a" then "1"
  // If the computation happens within `f`, then the order is correct.
  implicit val app = new Apply[TypedParams] {
    override def ap[A, B](fa: => TypedParams[A])(f: => TypedParams[(A) => B]): TypedParams[B] = {
      for {
        vf <- f
        va <- fa
      } yield va ap vf
    }

    override def map[A, B](fa: TypedParams[A])(f: (A) => B): TypedParams[B] = fa map (_ map f)
  }

}


/*
 * Errors should be something similar to:
 *
 * command("add")("A command that does something").error("Command should be `add`, you moron")
 *  .named[Int]("a")("First named parameter of type `Int`")
 *  .named[Int]("b")("Second named parameter of type `Int`")
 */

sealed trait Command[+A]

final case class Runner[A](types: TypedParams[A], syntax: Inter[Sym]) extends Command[A] {
  def run(params: List[String]): Result[A] = types run params _2
}

final case class P0(param0: Inter[Sym]) extends Command[Nothing] {
  def named[A](label: String)(implicit m: Reified[A]) = P1(reify(m), param0 affix Sym.named(label) infix Sym.typed[A])

  def unnamed[A](implicit m: Reified[A]) = P1(reify(m), param0 affix Sym.typed[A])

  def assignment[A](label: String, operator: String = "=")(implicit m: Reified[A]) = P1(reify(m), param0 affix Sym.assign(label, operator))

  final case class P1[A](type1: TypedParams[A], param1: Inter[Sym]) extends Command[A] {
    def named[B](label: String)(implicit m: Reified[B]) = P2(reify(m), param1 affix Sym.named(label) infix Sym.typed[B])

    def unnamed[B](implicit m: Reified[B]) = P2(reify(m), param1 affix Sym.typed[B])

    def assignment[B](label: String, operator: String = "=")(implicit m: Reified[B]) = P2(reify(m), param1 affix Sym.assign(label, operator))

    def apply[B](f: A => B) = Runner(app.map(type1)(f), param1)

    final case class P2[B](type2: TypedParams[B], param2: Inter[Sym]) extends Command[B] {
      def named[C](label: String)(implicit m: Reified[C]) = P1(reify(m), param2 affix Sym.named(label) infix Sym.typed[C])

      def unnamed[C](implicit m: Reified[C]) = P1(reify(m), param2 affix Sym.typed[C])

      def assignment[C](label: String, operator: String = "=")(implicit m: Reified[C]) = P1(reify(m), param2 affix Sym.assign(label, operator))

      def apply[C](f: (A, B) => C) = Runner(app.apply2(type1, type2)(f), param2)

      case class P3[C](type3: TypedParams[C], param3: Inter[Sym]) extends Command[C] {

        def apply[D](f: (A, B, C) => D) = Runner(app.apply3(type1, type2, type3)(f), param3)
      }

    }

  }

}