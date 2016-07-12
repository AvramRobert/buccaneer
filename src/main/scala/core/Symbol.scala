package core

object Symbol {

}

object Sym {
  def named(label: String): Inter[Sym] = Inter.lift(Named(label))

  def unnamed: Inter[Sym] = Inter.lift(Unnamed)
}

sealed trait Sym {
  def isNamed: Boolean = this match {
    case Named(_) => true
    case _ => false
  }

  def notNamed: Boolean = this match {
    case Unnamed => true
    case _ => false
  }
}

case class Named(label: String) extends Sym

case object Unnamed extends Sym