package core

sealed trait Sym {

  def isSymbol(s: String): Boolean = find(_ == s).fold(false)(_ => true)

  def | (that: String): Alternative = this | Label(that)

  def | (label: Label): Alternative = this match {
    case x @ Label(_) => Alternative(Vector(x, label))
    case Alternative(alt) => Alternative(alt :+ label)
  }

  final def find(p: String => Boolean): Option[Label] = this match {
    case x@Label(value) if p(value) => Some(x)
    case Alternative(alts) => alts.find(x => p(x.value))
    case _ => None
  }

  def show: String = this match {
    case Label(v) => v
    case Alternative(alts) => alts.map(_.value).mkString(" | ")
  }
}

case class Label(value: String) extends Sym

case class Alternative(alts: Vector[Label]) extends Sym

object Denot {
  def id(symbol: Sym, isMajor: Boolean = false, docs: Docs = Docs.empty): Identifier = Identifier(symbol, isMajor, docs)

  def typing[A](proof: Read[A], docs: Docs = Docs.empty): Typing[A] = Typing(proof, docs)

  def typedId[A](symbol: Sym, proof: Read[A], docs: Docs = Docs.empty): TypedIdentifier[A] = TypedIdentifier(symbol, proof, docs)
}

sealed trait Denot {
  def isTyped: Boolean = this match {
    case Typing(_, _) | TypedIdentifier(_, _, _) => true
    case _ => false
  }

  def isMajorIdentifier: Boolean = this match {
    case Identifier(_, isMajor, _) => isMajor
    case _ => false
  }

  def docs: Docs

  def msg(info: String): Denot

  def mapDocs(f: Docs => Docs): Denot = this match {
    case id@Identifier(_, _, docs) => id.copy(docs = f(docs))
    case typing@Typing(_, docs) => typing.copy(docs = f(docs))
    case typedId@TypedIdentifier(_, _, docs) => typedId.copy(docs = f(docs))
  }

  def show: String = this match {
    case Typing(_, _) => "<value>"
    case Identifier(sym, _, _) => sym.show
    case TypedIdentifier(sym, _, _) => s"${sym.show}<value>"
  }
}

case class Identifier(symbol: Sym, isMajor: Boolean, docs: Docs) extends Denot {
  override def msg(info: String): Identifier = Identifier(symbol, isMajor, docs.mapMsg(_ => info))
}

case class Typing[A](proof: Read[A], docs: Docs) extends Denot {
  override def msg(info: String): Typing[A] = Typing(proof, docs.mapMsg(_ => info))
}

case class TypedIdentifier[A](symbol: Sym, proof: Read[A], docs: Docs) extends Denot {
  override def msg(info: String): TypedIdentifier[A] = TypedIdentifier(symbol, proof, docs.mapMsg(_ => info))
}

object Docs {
  def empty: Docs = Docs("")
}

case class Docs(msg: String) {
  def mapMsg(f: String => String): Docs = copy(msg = f(msg))
}