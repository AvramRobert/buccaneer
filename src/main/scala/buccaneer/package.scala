import scala.util.control.NonFatal
import scalaz.{Applicative, Apply, Bind, Traverse, ValidationNel}
import scalaz.syntax.validation._

package object buccaneer {
  type Args = List[String]
  type Expr[A] = Vector[Denotation[A]]
  type AST[A] = Vector[(Denotation[A], String)]
  type SAST[A] = Vector[(Denotation[A], Option[String])]
  type Result[A] = ValidationNel[Throwable, A]

  def success[A](a: A): Result[A] = a.successNel[Throwable]
  def failure[A](t: Throwable): Result[A] = t.failureNel[A]
  def failure[A](message: String): Result[A] = failure(new Throwable(message))
  def attempt[A](eff: => A): Result[A] = try { success(eff) } catch { case NonFatal(t) => failure(t) }

  implicit lazy val traverseVector: Traverse[Vector] = new Traverse[Vector] {
    override def traverseImpl[G[_], A, B](fa: Vector[A])(f: (A) => G[B])(implicit ap: Applicative[G]): G[Vector[B]] = {
      fa.foldLeft(ap.point(Vector[B]())) { (gvb, a) =>
        ap.apply2(gvb, f(a))(_ :+ _)
      }
    }
  }

  implicit lazy val bindStep: Bind[Step] = new Bind[Step] {
    override def bind[A, B](fa: Step[A])(f: (A) => Step[B]) = fa flatMap f

    override def map[A, B](fa: Step[A])(f: (A) => B) = fa map f
  }

  implicit lazy val applyResult: Apply[Result] = new Apply[Result] {
    override def ap[A, B](fa: => Result[A])(f: => Result[(A) => B]) = fa ap f

    override def map[A, B](fa: Result[A])(f: (A) => B) = fa map f
  }
}
