package core

object Sym {
  def alt(label: String, desc: String): Sym = Alt("-" + label, "--" + label).mapMsg(_ => desc)

  def command(label: String, desc: String): Sym = Com(label).mapMsg(_ => desc)

  def named(label: String, desc: String): Sym = Named(label).mapMsg(_ => desc)

  def typed[A](implicit m: Reified[A]): Sym = Type[A](m)

  def assign(label: String, operator: String, desc: String): Sym = Assign(label, operator).mapMsg(_ => desc)
}

object Docs {
  def empty: Docs = Docs("", "")
}

case class Docs(description: String, errorMessage: String) {
  def mapMsg(f: String => String): Docs = fold(f)(identity)
  def mapErr(f: String => String): Docs = fold(identity)(f)
  def fold(desc: String => String)(err: String => String): Docs = Docs(desc(description), err(errorMessage))
}

//Currently, a `Com` is syntactically equivalent to a `Named`. Semantically they differ only slightly.
//The differentiation is to ease its specificity. In this regard, either change `Com`s semantics to better
//identify it within this algebra, or remove it all together and use `Named` instead.
sealed trait Sym {
  def isTyped: Boolean = this match {
    case Type(_) | Assign(_, _, _) => true
    case _ => false
  }

  def isNamed: Boolean = this match {
    case Named(_, _) => true
    case _ => false
  }

  def isAssigned: Boolean = this match {
    case Assign(_, _, _) => true
    case _ => false
  }

  def isCommand: Boolean = this match {
    case Com(_, _) => true
    case _ => false
  }

  def isAlt: Boolean = this match {
    case Alt(_, _, _) => true
    case _ => false
  }

  def foldDocs(desc: String => String)(err: String => String): Sym = this match {
    case Com(l, dcs) => Com(l, dcs.fold(desc)(err))
    case Named(l, dcs) => Named(l, dcs.fold(desc)(err))
    case Assign(l, op, dcs) => Assign(l, op, dcs.fold(desc)(err))
    case Alt(ths, tht, dcs) => Alt(ths, tht, dcs.fold(desc)(err))
    case _ => this
  }

  def mapMsg(f: String => String): Sym = foldDocs(f)(identity)
  def mapErr(f: String => String): Sym = foldDocs(identity)(f)

}
case class Com(label: String, docs: Docs = Docs.empty) extends Sym
case class Named(label: String, docs: Docs = Docs.empty) extends Sym
case class Assign(label: String, operator: String, docs: Docs = Docs.empty) extends Sym
case class Type[A](proof: Reified[A]) extends Sym
case class Alt(ths: String, tht: String, docs: Docs = Docs.empty) extends Sym