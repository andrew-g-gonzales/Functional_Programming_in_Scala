package fpinscala.ch3.datastructures.list

import fpinscala.ch3.datastructures.list._
import org.scalatest.funsuite.AnyFunSuite

class TestList extends AnyFunSuite{

  test("Test tail() implementation"){

    val ds = List(1,2,3,4,5,6,7,8,9)
    val tail = List.tail(ds)
    assertResult(List(2,3,4,5,6,7,8,9))(tail)
    println(tail)
  }

  test("Test Pattern Matching"){

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4,_))))=> x+y
      case Cons(h,t) => h + List.sum(t)
      case _ =>101
    }

    assertResult(3)(x)
    println(x)
  }

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
