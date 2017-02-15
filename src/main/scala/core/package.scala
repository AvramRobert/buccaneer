import core.Read.Result
import scalaz.{Applicative, Apply, Bind, Traverse}

package object core {

  implicit lazy val traverseVector: Traverse[Vector] = new Traverse[Vector] {
    override def traverseImpl[G[_], A, B](fa: Vector[A])(f: (A) => G[B])(implicit ap: Applicative[G]): G[Vector[B]] = {
      fa.foldLeft(ap.point(Vector[B]())) { (gvb, a) =>
        ap.apply2(gvb, f(a))(_ :+ _)
      }
    }
  }

  implicit lazy val bindStep = new Bind[Step] {
    override def bind[A, B](fa: Step[A])(f: (A) => Step[B]) = fa flatMap f

    override def map[A, B](fa: Step[A])(f: (A) => B) = fa map f
  }

  implicit lazy val lexicalChar: Lexical[Char] = new Lexical[Char] {
    override def blank = ' '

    override def break = '\n'

    override def continuation = '-'

    override def eq(a1: Char, a2: Char) = a1 == a2
  }

  implicit lazy val applyResult: Apply[Result] = new Apply[Result] {
    override def ap[A, B](fa: => Result[A])(f: => Result[(A) => B]) = fa ap f

    override def map[A, B](fa: Result[A])(f: (A) => B) = fa map f
  }
}
