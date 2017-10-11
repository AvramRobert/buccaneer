package buccaneer

object RoseList { self =>
  def empty[A]: RoseList[A] = Stem

  def prepend[A](roseList: RoseList[A], as: A*): RoseList[A] = as match {
    case Seq() => roseList
    case Seq(a) => Petal(a, roseList)
    case seq => Bundle(seq.toList, roseList)
  }

  def expand[A](roseList: RoseList[A]): Set[List[A]] = {
    def go(rem: RoseList[A], cur: List[A] = List.empty, total: Set[List[A]] = Set.empty): Set[List[A]] = rem match {
      case Petal(a, t) => go(t, a :: cur, total)
      case Bundle(as, t) =>
        total ++ as.permutations.toSet.flatMap { perm: List[A] => go (t, perm ::: cur) }
      case Stem => total + cur
    }
    go(roseList)
  }

  implicit class RoseListOps[A](roseList: RoseList[A]) {
    def prepend(as: A*): RoseList[A] = self.prepend(roseList, as: _*)
    def expand: Set[List[A]] = self.expand(roseList)
  }
}

sealed trait RoseList[+A]

case class Petal[A](head: A, tail: RoseList[A]) extends RoseList[A]

case class Bundle[A](head: List[A], tail: RoseList[A]) extends RoseList[A]

case object Stem extends RoseList[Nothing]