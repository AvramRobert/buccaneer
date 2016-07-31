package core


object Reified {
  def apply[A](f: String => A): Reified[A] = new Reified[A] {
    override def apply(a: String): A = f(a)
  }
}

trait Reified[A] {
  def apply(a: String): A
}
