package buccaneer

/** An ADT for representing symbols in a command line syntax
  */
sealed trait Sym {

  def | (that: String): Alternative = this match {
    case Label(v) => Alternative(Seq(v, that))
    case Alternative(alts) => Alternative(alts :+ that)
  }

  def | (label: Label): Alternative = this | label.value

  final def find(p: String => Boolean): Option[Label] = this match {
    case x@Label(value) if p(value) => Some(x)
    case Alternative(alts) => alts.find(p).map(Label)
    case _ => None
  }

  final def exists(p: String => Boolean): Boolean = find(p).fold(false)(_ => true)

  def show: String = this match {
    case Label(v) => v
    case Alternative(alts) => alts.mkString(" | ")
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
case class Alternative(alts: Seq[String]) extends Sym

object Denot {
  def id(symbol: Sym, isMajor: Boolean = false, docs: String = ""): Identifier = Identifier(symbol, isMajor, docs)

  def typing[A](proof: Read[A], docs: String = ""): Typing[A] = Typing(proof, docs)

  def typedId[A](symbol: Sym, proof: Read[A], docs: String = ""): TypedIdentifier[A] = TypedIdentifier(symbol, proof, docs)
}

/** An ADT for representing denotations in a command line syntax.
  */
sealed trait Denot {
  def docs: String

  def msg(info: String): Denot

  def isTyped: Boolean = this match {
    case Typing(_, _) | TypedIdentifier(_, _, _) => true
    case _ => false
  }

  def isMajorIdentifier: Boolean = this match {
    case Identifier(_, isMajor, _) => isMajor
    case _ => false
  }

  def show: String = this match {
    case Typing(proof, _) => s"<${proof.show}>"
    case Identifier(sym, _, _) => sym.show
    case TypedIdentifier(Label(v), proof, _) => s"$v<${proof.show}>"
    case TypedIdentifier(Alternative(alts), proof, _) => Alternative(alts.map(v => s"$v<${proof.show}>")).show
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
case class Identifier(symbol: Sym, isMajor: Boolean, docs: String = "") extends Denot {
  override def msg(info: String): Identifier = Identifier(symbol, isMajor, info)
}

/** Represents a construct that denotes a type in a command line syntax.
  *
  * @param proof instance of the `Read` typeclass that dictates how the input
  *              string is to be converted to the expected type
  * @param docs documentation information about the type
  * @tparam A the expected type
  */
case class Typing[A](proof: Read[A], docs: String = "") extends Denot {
  override def msg(info: String): Typing[A] = Typing(proof, info)
}

/** Represents a construct that denotes the association between a key value and a type.
  *
  * @param symbol concrete symbol association with this denotation
  * @param proof instance of the `Read` typeclass that dictates how the input
  *              string is to be converted to the expected type
  * @param docs documentation information about the association
  * @tparam A the expected type
  */
case class TypedIdentifier[A](symbol: Sym, proof: Read[A], docs: String = "") extends Denot {
  override def msg(info: String): TypedIdentifier[A] = TypedIdentifier(symbol, proof, info)
}