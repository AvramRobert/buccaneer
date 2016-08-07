package core

object Sym {
  def command(label: String, desc: String): Inter[Sym] = Inter.lift(Com(label).mapMsg(_ => desc))

  def named(label: String, desc: String): Inter[Sym] = Inter.lift(Named(label).mapMsg(_ => desc))

  def typed[A](implicit m: Reified[A]): Inter[Sym] = Inter.lift(Type[A](m))

  def assign(label: String, operator: String, desc: String): Inter[Sym] = Inter.lift(Assign(label, operator).mapMsg(_ => desc))
}

object Docs {
  def empty: Docs = Docs("", "")
}

case class Docs(description: String, errorMessage: String) {
  def mapMsg(f: String => String): Docs = fold(f)(identity)
  def mapErr(f: String => String): Docs = fold(identity)(f)
  def fold(desc: String => String)(err: String => String): Docs = Docs(desc(description), err(errorMessage))
}

// TODO: Each symbol should have some `Docs` element
sealed trait Sym {
  def isTyped: Boolean = this match {
    case Type(_) | Assign(_, _, _) => true
    case _ => false
  }

  def foldDocs(desc: String => String)(err: String => String): Sym = this match {
    case Com(l, dcs) => Com(l, dcs.fold(desc)(err))
    case Named(l, dcs) => Named(l, dcs.fold(desc)(err))
    case Assign(l, op, dcs) => Assign(l, op, dcs.fold(desc)(err))
    case _ => this
  }

  def mapMsg(f: String => String): Sym = foldDocs(f)(identity)
  def mapErr(f: String => String): Sym = foldDocs(identity)(f)

}
case class Com(label: String, docs: Docs = Docs.empty) extends Sym
case class Named(label: String, docs: Docs = Docs.empty) extends Sym
case class Assign(label: String, operator: String, docs: Docs = Docs.empty) extends Sym
case class Type[A](proof: Reified[A]) extends Sym
