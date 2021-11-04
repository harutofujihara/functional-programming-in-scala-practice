package datastructures

import org.scalatest._
import datastructures.Tree._
import datastructures.Tree.{size => sizeTree}

class TreeSpec extends FlatSpec with Matchers {
  "size" should "count node size" in {
    val leaf1 = Leaf("a")
    val leaf2 = Leaf("b")
    val leaf3 = Leaf("c")
    val leaf4 = Leaf("d")

    val branch1 = Branch(leaf1,leaf2)
    val branch2 = Branch(leaf3,leaf4)
    val root = Branch(branch1, branch2)

    sizeTree(root) shouldEqual 7
  }

  "maximum" should "get max value" in {
    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val leaf3 = Leaf(4)
    val leaf4 = Leaf(3)

    val branch1 = Branch(leaf1,leaf2)
    val branch2 = Branch(leaf3,leaf4)
    val root = Branch(branch1, branch2)

    maximum(root) shouldEqual 4
  }

  "map" should "mapping function on each node" in {
    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val root = Branch(leaf1,leaf2)

    val mappedTree = map(root)(a => a + a)

    mappedTree match {
      case Branch(l, r) => (l, r) match {
        case (Leaf(lv), Leaf(rv)) => {
          lv shouldEqual 2
          rv shouldEqual 4
        }
        case _ => throw new Exception()
      }
      case _ => throw new Exception()
    }
  }
}
