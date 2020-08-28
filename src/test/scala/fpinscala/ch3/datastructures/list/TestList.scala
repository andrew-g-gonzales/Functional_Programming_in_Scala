package fpinscala.ch3.datastructures.list

import fpinscala.ch3.datastructures.list._
import org.scalatest.funsuite.AnyFunSuite

class TestList extends AnyFunSuite{

  test("Test sum()"){

    val nums:List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
    //println(nums)
    val result = List.sum(nums)
    println(result)
    assertResult(6)(result)
  }

  test("Test product"){

    val nums:List[Double] = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0,Nil))))
    val result = List.product(nums)
    println(result)
    assertResult(24)(result)
  }
}
