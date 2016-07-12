package core


object Mapper {
  def apply[A, B](f: A => B): Mapper[A, B] = new Mapper[A, B] {
    override def apply(a: A): B = f(a)
  }
}

trait Mapper[A, B] {
  def apply(a: A): B
}
