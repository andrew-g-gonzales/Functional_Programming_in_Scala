package fpinscala.ch3.datastructures.tree

import fpinscala.ch3.datastructures.list._
import org.scalatest.funsuite.AnyFunSuite

class TestTree extends AnyFunSuite {

  test("Testing 3.27: Write a function depth that returns the maximum path length from the root of a tree to any leaf."){

    val tree = Branch(
      Branch(
        Branch(Leaf(1),Leaf(2)),Leaf(3)),
      Branch(Leaf(9),
        Branch(Leaf(33),Leaf(6)))
    )
    val depth = Tree.depth(tree)
    assertResult(3)(depth)
    println(depth)
  }

  test("Testing 3.26: Write a function maximum that returns the maximum element in a Tree[Int]."){

    val tree = Branch(
      Branch(
        Branch(Leaf(1),Leaf(2)),Leaf(3)),
      Branch(Leaf(9),
        Branch(Leaf(33),Leaf(6)))
    )
    val maxVal = Tree.maximum(tree)
    assertResult(33)(maxVal)
    println(maxVal)
  }

  test("Testing 3.25: Write a function size that counts the number of nodes (leaves and branches) in a tree."){

    val tree = Branch(
      Branch(
        Branch(Leaf(1),Leaf(2)),Leaf(3)),
      Branch(Leaf(9), Branch(Leaf(5),Leaf(6)))
    )
    val size = Tree.size(tree)
    assertResult(11)(size)
    println(size)
  }

}
