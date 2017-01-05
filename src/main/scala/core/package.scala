import scalaz.{Applicative, Traverse}
import core.Lexical

package object core {

  implicit lazy val traverseVector: Traverse[Vector] = new Traverse[Vector] {
    override def traverseImpl[G[_], A, B](fa: Vector[A])(f: (A) => G[B])(implicit ap: Applicative[G]): G[Vector[B]] = {
      fa.foldLeft(ap.point(Vector[B]())) { (gvb, a) =>
        ap.apply2(gvb, f(a))(_ :+ _)
      }
    }
  }

  implicit lazy val lexicalChar: Lexical[Char] = new Lexical[Char] {
    override def blank = ' '

    override def break = '\n'

    override def continuation = '-'

    override def eq(a1: Char, a2: Char) = a1 == a2
  }

}
