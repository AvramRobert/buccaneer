package buccaneer

object Denotation {
  def proof[A: Read]: Read[A] = implicitly[Read[A]]
  def command(label: String): Com = Com(label, "")
  def option(labels: String*): Opt = Opt(labels.toList, "")
  def argument[A: Read]: Arg[A] = Arg(proof[A], "")
  def assignment[A: Read](op: String, labels: String*): Assgn[A] = Assgn(labels.toList, op, proof[A], "")
}

sealed trait Denotation[+A] {
  def msg(desc: String): Denotation[A] = this match {
    case Com(label, _) => Com(label, desc)
    case Opt(labels, _) => Opt(labels, desc)
    case Arg(read, _) => Arg(read, desc)
    case Assgn(read, labels, op, _) => Assgn(read, labels, op, desc)
  }

  def isTyped: Boolean = this match {
    case Arg(_, _) | Assgn(_, _, _, _) => true
    case _ => false
  }
}
case class Com(label: String, description: String) extends Denotation[Nothing]
case class Opt(labels: List[String], description: String) extends Denotation[Nothing]
case class Arg[A](read: Read[A], description: String) extends Denotation[A]
case class Assgn[A](labels: List[String], op: String, read: Read[A], description: String) extends Denotation[A]
