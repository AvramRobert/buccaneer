import scala.util.control.NonFatal
import scalaz.{\/, \/-, -\/, Bind, Traverse}
import scalaz.std.list.listInstance

package object buccaneer {
  type Args = List[String]
  type Expr[A] = List[Denotation[A]]
  type AST[A] = List[(Denotation[A], Option[String])]
  type Result[A] = \/[Throwable, A]

  def success[A](a: A): Result[A] = \/-(a)
  def failure[A](t: Throwable): Result[A] = -\/(t)
  def failure[A](message: String): Result[A] = failure(new Throwable(message))
  def attempt[A](eff: => A): Result[A] = try { success(eff) } catch { case NonFatal(t) => failure(t) }

  implicit lazy val traverseList: Traverse[List] = listInstance

  implicit lazy val bindStep: Bind[Step] = new Bind[Step] {
    override def bind[A, B](fa: Step[A])(f: (A) => Step[B]) = fa flatMap f

    override def map[A, B](fa: Step[A])(f: (A) => B) = fa map f
  }
}
