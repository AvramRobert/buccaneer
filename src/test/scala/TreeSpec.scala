import core.{Leaf, Node, Tree}
import org.scalacheck.Gen
import scala.util.Random
import scalaz.std.option._

class TreeSpec extends DefaultTestSuite {

  "A binary tree" should {

    "be rightist by means of affixing" in {
      val values = List(1, 2, 3, 4)
      val rightist = Node(1, Leaf, Node(2, Leaf, Node(3, Leaf, Node(4))))

      rightist shouldBe values.foldLeft(Tree.empty[Int])(_ affix _)
    }

    "be leftist by means of infixing" in {
      val values = List(1, 2, 3, 4)
      val leftist = Node(1, Node(2, Node(3, Node(4))))

      leftist shouldBe values.foldLeft(Tree.empty[Int])(_ infix _)
    }

    "in this particular case be biased towards the right" in {
      val values = List(1, 2, 3, 4, 5)
      val rightistivity1 = Node(1, Leaf, Node(2, Node(3), Node(4, Node(5))))
      val rightistivity2 = Node(1, Node(2), Node(3, Node(4), Node(5)))

      rightistivity1 shouldBe values.foldLeft(Tree.empty[Int]) {
        (tree, i) =>
          if(i % 2 == 0) tree affix i
          else tree infix i
      }

      rightistivity2 shouldBe values.foldLeft(Tree.empty[Int]) {
        (tree, i) =>
          if(i % 2 == 0) tree infix i
          else tree affix i
      }
    }

    "satisfy the traversable identity law" in {
      forAll(trees(0, 1000)) { tree =>
        Tree.traverse.traverseLaw.identity(tree) shouldBe true
      }
    }

    "satisfy the traversable composition law" in {
      val f = (i: Int) => i + 1
      val g = (i: Int) => i - 1
      forAll(trees(0, 1000)) { tree =>
        Tree.traverse.traverseLaw.composite(tree, f, g) shouldBe true
      }
    }

    "satisfy the traversable identity traverse law" in {
      forAll(trees(0, 1000)) { tree =>
        Tree.traverse.traverseLaw.identityTraverse(tree, (i: Int) => i.toString) shouldBe true
      }
    }

    "satisfy the traversable invariant identity law" in {
      forAll(trees(0, 1000)) { tree =>
        Tree.traverse.traverseLaw.invariantIdentity(tree) shouldBe true
      }
    }

    "satisfy the traversable invariant composite law" in {
      val f = (i: Int) => i.toString
      val g = (i: String) => i.toInt
      val h = (i: String) => i.toDouble
      val j = (i: Double) => i.toString

      forAll(trees(0, 1000)) { tree =>
        Tree.traverse.traverseLaw.invariantComposite[Int, String, Double](tree, f, g, h, j) shouldBe true
      }
    }

    "satisfy the traversable purity law" in {
      forAll(trees(0, 1000)) { tree =>
        Tree.traverse.traverseLaw.purity[Option, Int](tree) shouldBe true
      }
    }

    "zip lists" in {
      forAll(trees(0, 1000), lists(0, 1000)) { (tree, list) =>
        (list containsSlice (tree zipWithList list).map(_._2).toVector.toList) shouldBe true
      }
    }

    "zip lists arbitrarily" in {
      forAll(trees(0, 1000), lists(0, 1000)) { (tree, list) =>
        whenever(tree.depth >= list.size) {
          (tree zips list).filter(_._2.isDefined).map(_._2.get) shouldBe list
        }
      }
    }

    "fold properly" in {
      forAll(trees(0, 1000)) { tree =>
        tree.foldLeft(0)(_ + _) shouldBe tree.toVector.sum
      }
    }

    "allow conversions to a string" in {
      forAll(trees(0, 1000), Gen.alphaLowerStr) { (tree, s) =>
        tree.string(s)(_.toString) shouldBe tree.toVector.mkString(s)
      }
    }
  }

  def lists[A](min: A, max: A)(implicit choose: Gen.Choose[A]): Gen[List[A]] = Gen.listOf(Gen.choose(min, max))

  def trees[A](min: A, max: A)(implicit choose: Gen.Choose[A]): Gen[Tree[A]] = lists(min, max) map {
    _.foldLeft(Tree.empty[A]) { (tree, a) =>
      if (Random.nextBoolean()) tree affix a
      else tree infix a
    }
  }

}