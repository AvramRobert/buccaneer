package core

object Sym {
  def named(label: String): Inter[Sym] = Inter.lift(Named(label))

  def typed[A](implicit m: Reified[A]): Inter[Sym] = Inter.lift(Type[A](m))

  def assign(label: String, operator: String): Inter[Sym] = Inter.lift(Assign(label, operator))
}

case class Docs(description: String, errorMessage: String)

// TODO: Each symbol should have some `Docs` element
sealed trait Sym {
  def docs: Docs = Docs("t", "a")

  def isTyped: Boolean = this match {
    case Type(_) | Assign(_, _) => true
    case _ => false
  }
}
case class Com(label: String) extends Sym
case class Named(label: String) extends Sym
case class Assign(label: String, operator: String) extends Sym
case class Type[A](proof: Reified[A]) extends Sym
