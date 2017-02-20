package core

/** An ADT for representing symbols in a command line syntax
  */
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

/** Represents a singular, unary symbol.
  *
  * @param value concrete symbol value
  */
case class Label(value: String) extends Sym

/** Represents the union of a multitude of syntactically different symbols, that are semantically equal.
  *
  * @param alts list of various unary symbols
  */
case class Alternative(alts: Vector[Label]) extends Sym

object Denot {
  def id(symbol: Sym, isMajor: Boolean = false, docs: Docs = Docs.empty): Identifier = Identifier(symbol, isMajor, docs)

  def typing[A](proof: Read[A], docs: Docs = Docs.empty): Typing[A] = Typing(proof, docs)

  def typedId[A](symbol: Sym, proof: Read[A], docs: Docs = Docs.empty): TypedIdentifier[A] = TypedIdentifier(symbol, proof, docs)
}

/** An ADT for representing denotations in a command line syntax.
  */
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

/** Represents a construct that denotes a simple key element in a command line syntax.
  * A `major=true` key element is considered to be either a command or subcommand identifier.
  *
  * @param symbol concrete symbol associated with this denotation
  * @param isMajor identifier `importance`. commands and subcommands are considered major,
  *                whilst the rest are not
  * @param docs documentation information about the identifier
  */
case class Identifier(symbol: Sym, isMajor: Boolean, docs: Docs) extends Denot {
  override def msg(info: String): Identifier = Identifier(symbol, isMajor, docs.mapMsg(_ => info))
}

/** Represents a construct that denotes a type in a command line syntax.
  *
  * @param proof instance of the `Read` typeclass that dictates how the input
  *              string is to be converted to the expected type
  * @param docs documentation information about the type
  * @tparam A the expected type
  */
case class Typing[A](proof: Read[A], docs: Docs) extends Denot {
  override def msg(info: String): Typing[A] = Typing(proof, docs.mapMsg(_ => info))
}

/** Represents a construct that denotes the association between a key value and a type.
  *
  * @param symbol concrete symbol association with this denotation
  * @param proof instance of the `Read` typeclass that dictates how the input
  *              string is to be converted to the expected type
  * @param docs documentation information about the association
  * @tparam A the expected type
  */
case class TypedIdentifier[A](symbol: Sym, proof: Read[A], docs: Docs) extends Denot {
  override def msg(info: String): TypedIdentifier[A] = TypedIdentifier(symbol, proof, docs.mapMsg(_ => info))
}

object Docs {
  def empty: Docs = Docs("")
}

/** Record for maintaining documentation.
  *
  * @param msg documentation content
  */
case class Docs(msg: String) {
  def mapMsg(f: String => String): Docs = copy(msg = f(msg))
}