package core

import core.Command._

import scalaz.syntax.validation._
import scala.util.{Failure, Success, Try}
import scalaz.{State, _}
import Binary.treeSyntax

object Command {
  type Result[A] = ValidationNel[Throwable, A]
  type TypedParams[A] = IndexedState[List[String], List[String], Result[A]]

  def apply(label: String, desc: String = ""): P0 = P0(Binary.liftTree(Sym.command(label, desc)))

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
  //if there are no typed parameters needed, the normalisation propagates an empty list to the monad
  //that is why i have to propagate a list with an empty string inside, if there are no types to parse and use
  def runnerState[B](f: String => B): TypedParams[B] = {
    for {
      p <- State.get[List[String]]
      _ <- State.modify[List[String]](_.tail)
      h = p.head
    } yield {
      Try(f(h)) match {
        case Success(b) => b.successNel
        case Failure(e) => e.failureNel
      }
    }
  }
}

// TODO: Try using sbt boilerplate for this: https://github.com/sbt/sbt-boilerplate
/*
 * Apparently spray does a similar thing.
 * They have a path builder, which builds some sort of path
 * by means of a builder `("path" / bla / bla)((a, b) => ... )`
 * and then they supply a function block whereby the number of arguments is equal to the
 * number of supplied sub-paths. And their types are (apparently) also consistent.
 * I believe however, that they don't lazily apply the hlist, but do it directly.
 * And that's why they might perhaps get more concrete type inference.
 */
sealed trait Command[+A] {
  def reify[B](f: Reified[B]): TypedParams[B] = runnerState[B](f.apply)
}

final case class Runner[A](types: TypedParams[A], syntax: Tree[Sym]) extends Command[A] {
  def map[B](f: A => B): Runner[B] = Runner[B](types.map(_ map f), syntax)

  def run(params: List[String]): Result[A] = types run params _2
}

final case class P0(param0: Tree[Sym]) extends Command[Nothing] {
  def named[A](label: String, desc: String = "")(implicit m: Reified[A]) = P1(reify(m), param0 affix Sym.named(label, desc) infix Sym.typed[A])

  def unnamed[A](implicit m: Reified[A]) = P1(reify(m), param0 affix Sym.typed[A])

  def assignment[A](label: String, operator: String = "=", desc: String = "")(implicit m: Reified[A]) = P1(reify(m), param0 affix Sym.assign(label, operator, desc))

  def option(label: String, desc: String = ""): P0 = P0(param0 affix Sym.alt(label, desc))

  def apply[A](f: Unit => A) = Runner(runnerState(_ => f(())), param0)

  final case class P1[A](type1: TypedParams[A], param1: Tree[Sym]) extends Command[A] {
    def named[B](label: String, desc: String = "")(implicit m: Reified[B]) = P2(reify(m), param1 affix Sym.named(label, desc) infix Sym.typed[B])

    def unnamed[B](implicit m: Reified[B]) = P2(reify(m), param1 affix Sym.typed[B])

    def assignment[B](label: String, operator: String = "=", desc: String = "")(implicit m: Reified[B]) = P2(reify(m), param1 affix Sym.assign(label, operator, desc))

    def apply[B](f: A => B) = Runner(app.map(type1)(f), param1)

    final case class P2[B](type2: TypedParams[B], param2: Tree[Sym]) extends Command[B] {
      def named[C](label: String, desc: String = "")(implicit m: Reified[C]) = P1(reify(m), param2 affix Sym.named(label, desc) infix Sym.typed[C])

      def unnamed[C](implicit m: Reified[C]) = P1(reify(m), param2 affix Sym.typed[C])

      def assignment[C](label: String, operator: String = "=", desc: String = "")(implicit m: Reified[C]) = P1(reify(m), param2 affix Sym.assign(label, operator, desc))

      def apply[C](f: (A, B) => C) = Runner(app.apply2(type1, type2)(f), param2)

      case class P3[C](type3: TypedParams[C], param3: Tree[Sym]) extends Command[C] {

        def apply[D](f: (A, B, C) => D) = Runner(app.apply3(type1, type2, type3)(f), param3)
      }

    }

  }

}