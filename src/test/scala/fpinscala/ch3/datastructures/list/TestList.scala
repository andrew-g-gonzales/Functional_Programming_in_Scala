package fpinscala.ch3.datastructures.list

import fpinscala.ch3.datastructures.list._
import org.scalatest.funsuite.AnyFunSuite

class TestList extends AnyFunSuite{

  test("Test dropWhile()"){

    val ds:List[Int] = List(1,2,3,4,5,6,7,8,9)
    val result = List.dropWhile(ds)(_ < 5)
    assertResult(List(5,6,7,8,9))(result)
    println(result)
  }

  test("Test dropIf()"){

    val ds:List[Int] = List(1,3,5,7,9,2,4,6,8)
    val result = List.dropIf(ds,(a:Int)=> a>=7)
    assertResult(List(1,3,5,2,4,6))(result)
    println(result)
  }

  test("Test init()"){

    val ds:List[Int] = List(1,2,3,4,5,6,7,8,9)
    val result = List.init(ds)
    assertResult(List(1,2,3,4,5,6,7,8))(result)
    println(result)
  }

  test("Test append() : two full lists"){

    val a1 = List(8,6,7)
    val a2 = List(5,3,0,9)
    val result = List.append(a1,a2)
    assertResult(result)(List(8,6,7,5,3,0,9))
    println(result)
  }

  test("Test append() : 1 element to full list"){

    val a1 = List(8)
    val a2 = List(6,7,5,3,0,9)
    val result = List.append(a1,a2)
    assertResult(result)(List(8,6,7,5,3,0,9))
    println(result)
  }

  test("Test append() : Nil to full list"){

    val a1 = Nil
    val a2 = List(8,6,7,5,3,0,9)
    val result = List.append(a1,a2)
    assertResult(result)(List(8,6,7,5,3,0,9))
    println(result)
  }

  test("Test drop() 3 from 9 elements"){

    val ds:List[Int] = List(1,2,3,4,5,6,7,8,9)
    val result = List.drop(ds,3)
    assertResult(List(4,5,6,7,8,9))(result)
    println(result)
  }

  test("Test drop() 8 from 9 elements"){

    val ds:List[Int] = List(1,2,3,4,5,6,7,8,9)
    val result = List.drop(ds,8)
    assertResult(List(9))(result)
    println(result)
  }

  test("Test drop() 9 from 9 elements"){

    val ds:List[Int] = List(1,2,3,4,5,6,7,8,9)
    val result = List.drop(ds,9)
    assertResult(Nil)(result)
    println(result)
  }

  test("Test drop() 10 from 9 elements"){

    val ds:List[Int] = List(1,2,3,4,5,6,7,8,9)
    val result = List.drop(ds,10)
    assertResult(Nil)(result)
    println(result)
  }

  test("Test setHead() with 9 elements"){

    val ds:List[Int] = List(2,2,3,4,5,6,7,8,9)
    val result = List.setHead(ds,1)
    assertResult(List(1,2,3,4,5,6,7,8,9))(result)
    println(result)
  }

  test("Test setHead() with Nil"){

    val ds:List[Int] = Nil
    val result = List.setHead(ds,1)
    assertResult(List(1))(result)
    println(result)
  }

  test("Test setHead() with 1 element"){

    val ds:List[Int] = List(10)
    val result = List.setHead(ds,1)
    assertResult(List(1))(result)
    println(result)
  }

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
