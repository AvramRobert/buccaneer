package buccaneer

import shapeless.DepFn1

trait Fixer[A] extends DepFn1[A]

object Fixer extends LowPriorityImplicits {
  type Aux[A, Out0] = Fixer[A] { type Out = Out0 }

  def fix[A, B](f: A => B): Aux[A, B] = new Fixer[A] {
    override type Out = B
    override def apply(t: A): Out = f(t)
  }
  def id[A]: Aux[A, A] = fix[A, A](identity)

  implicit def fix1[A]: Aux[Tuple1[A], A] = fix[Tuple1[A], A](_._1)
}

trait LowPriorityImplicits {
  implicit def fixN[A]: Fixer.Aux[A, A] = Fixer.id
}
