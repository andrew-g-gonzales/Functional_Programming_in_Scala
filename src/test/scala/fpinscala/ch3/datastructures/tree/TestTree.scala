package fpinscala.ch3.datastructures.tree

import fpinscala.ch3.datastructures.list._
import org.scalatest.funsuite.AnyFunSuite

class TestTree extends AnyFunSuite {



  test("Testing 3.28: Write a function map, analogous to the method of the same name on List, that modifies each element " +
    "in a tree with a given function."){

    val tree = Branch(
      Branch(
        Branch(Leaf(2),Leaf(4)),Leaf(6)),
      Branch(Leaf(8), Branch(Leaf(10),Leaf(12)))
    )

    val mappedTree = Tree.map(tree)(_*2)
    assertResult(Branch(
      Branch(
        Branch(Leaf(4),Leaf(8)),Leaf(12)),
      Branch(Leaf(16), Branch(Leaf(20),Leaf(24)))
    ))(mappedTree)
    println(mappedTree)
  }

  test("Testing 3.29: depth function using fold()"){

    val tree = Branch(
      Branch(
        Branch(Leaf(1),Leaf(2)),Leaf(3)),
      Branch(Leaf(9),
        Branch(Leaf(33),Leaf(6)))
    )
    val depth = Tree.depthViaFold(tree)
    assertResult(3)(depth)
    println(depth)
  }

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

  test("Testing 3.29: map function using fold()"){

    val tree = Branch(
      Branch(
        Branch(Leaf(1),Leaf(2)),Leaf(3)),
      Branch(Leaf(9),
        Branch(Leaf(33),Leaf(6)))
    )

    val doubled = Tree.map(tree)(_*2)

    assertResult( Branch(
      Branch(
        Branch(Leaf(2),Leaf(4)),Leaf(6)),
      Branch(Leaf(18),
        Branch(Leaf(66),Leaf(12)))
    ))(doubled)
    println(doubled)
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

  test("Testing 3.29: size function using fold()"){

    val tree = Branch(
      Branch(
        Branch(Leaf(1),Leaf(2)),Leaf(3)),
      Branch(Leaf(9),
        Branch(Leaf(33),Leaf(6)))
    )
    val size = Tree.fold(tree)((a:Int,b:Int) => 1+(a + b))(_=>1)
    assertResult(11)(size)
    println(size)
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
