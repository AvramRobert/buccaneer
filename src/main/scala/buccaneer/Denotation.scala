package buccaneer

sealed trait Denotation[+A] {
  val description: String
  def msg(desc: String): Denotation[A]

  def isTyped: Boolean = this match {
    case Arg(_, _) | Assgn(_, _, _, _) => true
    case _ => false
  }

  def isCommand: Boolean = this match {
    case Com(_, _) => true
    case _ => false
  }

  def isOption: Boolean = this match {
    case Opt(_, _) => true
    case _ => false
  }

  def show: String = this match {
    case Com(label, _) => label
    case Opt(labels, _) => labels.mkString("|")
    case Arg(read, _) => s"<${read.show}>"
    case Assgn(labels, op, read, _) => labels.map(s => s"$s$op<${read.show}>").mkString("|")
  }

  override def toString: String = show
}
case class Com(label: String, description: String) extends Denotation[Nothing] {
  def msg(desc: String): Com = Com(label, desc)
}
case class Opt(labels: List[String], description: String) extends Denotation[Nothing] {
  def msg(desc: String): Opt = Opt(labels, desc)
}
case class Arg[A](read: Read[A], description: String) extends Denotation[A] {
  def msg(desc: String): Arg[A] = Arg(read, desc)
}
case class Assgn[A](labels: List[String], op: String, read: Read[A], description: String) extends Denotation[A] {
  def msg(desc: String): Assgn[A] = Assgn(labels, op, read, desc)
}
