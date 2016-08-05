package core

object Implicits {
  implicit val reifiedInt: Reified[Int] = Reified[Int](_.toInt)
  implicit val reifiedBool: Reified[Boolean] = Reified[Boolean](_.toBoolean)
  implicit val reifiedString: Reified[String] = Reified[String](identity)
}
