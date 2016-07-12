package core

import core.Command._
import core.Types._

import scala.util.Try
import scalaz.{Apply, Bind}

object Command {

  def apply[A](label: String): P0 = P0(Inter.lift(Named(label)))

  implicit val tryB = new Bind[Try] {
    override def bind[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa flatMap f

    override def map[A, B](fa: Try[A])(f: (A) => B): Try[B] = fa map f
  }

  implicit val app = new Apply[TypedParams] {
    override def ap[A, B](fa: => TypedParams[A])(f: => TypedParams[(A) => B]): TypedParams[B] = {
      for {
        va <- fa
        vf <- f
      } yield va ap vf
    }

    override def map[A, B](fa: TypedParams[A])(f: (A) => B): TypedParams[B] = fa map (_ map f)
  }
}

sealed trait Command[+A]

case class Runner[A](types: TypedParams[A], syntax: Inter[Sym]) extends Command[A]

case class P0(param0: Inter[Sym]) extends Command[Nothing] {
  def named[A](label: String)(implicit m: Mapper[String, A]) = P1(morph(m.apply), param0 affix Sym.named(label) infix Sym.unnamed)

  case class P1[A](type1: TypedParams[A], param1: Inter[Sym]) extends Command[A] {
    def named[B](label: String)(implicit m: Mapper[String, B]) = P2(morph(m.apply), param1 affix Sym.named(label) infix Sym.unnamed)

    def apply[B](f: A => B) = Runner(app.map(type1)(f), param1)


    case class P2[B](type2: TypedParams[B], param2: Inter[Sym]) extends Command[B] {
      def named[C](label: String)(implicit m: Mapper[String, C]) = P1(morph(m.apply), param2 affix Sym.named(label) infix Sym.unnamed)

      def apply[C](f: (A, B) => C) = Runner(app.apply2(type1, type2)(f), param2)

      case class P3[C](type3: TypedParams[C], param3: Inter[Sym]) extends Command[C] {

        def apply[D](f: (A, B, C) => D) = Runner(app.apply3(type1, type2, type3)(f), param3)
      }

    }

  }

}