package fpinscala.ch3.datastructures.list

import fpinscala.ch3.datastructures.list._
import org.scalatest.funsuite.AnyFunSuite

class TestList extends AnyFunSuite{

  test("Testing 3.23: Generalize the function you just wrote so that it’s not specific to integers or addition."){

    val list1 = List("A", "B", "C")
    val list2 = List(1, 2, 3)
    val zipped = List.zipWith(list1,list2)((a,b)=> s"$a$b")
    assertResult(List("A1", "B2", "C3"))(zipped)
    println(zipped)
  }

  test("Testing 3.22: Write a function that accepts two lists and constructs a new list by adding corresponding " +
    "elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9). "){

    val (list1,list2) = (List(1,2,3), List(4,5,6))
    val addedPairWise = List.addPairWise(list1,list2)
    assertResult(List(5,7,9))(addedPairWise)
    println(addedPairWise)
  }

  test("Testing 3.21: Use flatMap to implement filter."){

    val list = List(8,6,7,5,3,0,9)
    val filtered =List.filterViaFlatMap(list)(_>5)
    assertResult(List(8,6,7,9))(filtered)
    println(filtered)
  }

  test("Testing 3.20: Write a function flatMap that works like map except that the function given will return " +
    "a list instead of a single result, and that list should be inserted into the final resulting list"){

    val list = List(1,2,3)
    val flatMapped = List.flatMap(list)(i=>List(i,i))
    assertResult(List(1,1,2,2,3,3))(flatMapped)
    println(flatMapped)
  }

  test("Testing 3.18: Write a function map that generalizes modifying each element in a list while maintaining " +
    "the structure of the list.  Using Fold Right"){

    val list = List(1,2,3,4,5,6,7,8,9,10)
    val doubled = List.map2(list)(_*2)
    assertResult(List(2,4,6,8,10,12,14,16,18,20))(doubled)
    println(doubled)
  }


  test("Testing 3.18: Write a function map that generalizes modifying each element in a list while maintaining " +
    "the structure of the list."){

    val list = List(1,2,3,4,5,6,7,8,9,10)
    val doubled = List.map(list)(_*2)
    assertResult(List(2,4,6,8,10,12,14,16,18,20))(doubled)
    println(doubled)
  }

  test("Testing 3.17: Write a function that turns each value in a List[Double] into a String."){

    val list = List(1.1,2.2,3.3,4.4,5.5,6.6,7.7,8.8,9.9)
    val stringified = List.stringifyDoubleList(list)
    assertResult(List("1.1","2.2","3.3","4.4","5.5","6.6","7.7","8.8","9.9"))(stringified)
    println(stringified)
  }

  test("Testing 3.16: adding 1 to a list, returning a new list"){

    val list = List(1,2,3,4,5,6,7,8,9)
    val newList = List.add1(list)
    assertResult(List(2,3,4,5,6,7,8,9,10))(newList)
    println(newList)
  }

  test("Testing 3.15: Concatenate list of lists with functions already used "){

    val listOfLists = List(List(1,2,3), List(4,5,6), List(7,8,9))
    val list = List.flatten(listOfLists)
    assertResult(List(1,2,3,4,5,6,7,8,9))(list)
    println(list)
  }

  test("Test 3.14: append function with foldLeft "){

    val list1 = List(1,2,3,4,5)
    val list2 = List(6,7,8,9,10)
    val appended =List.append2(list1,list2)
    assertResult(List(1,2,3,4,5,6,7,8,9,10))(appended)
    println(appended)
  }

  test("Test 3.14: append function with foldRight"){

    val list1 = List(1,2,3,4,5)
    val list2 = List(6,7,8,9,10)
    val appended =List.append1(list1,list2)
    assertResult(List(1,2,3,4,5,6,7,8,9,10))(appended)
    println(appended)
  }

  test("Testing 3.12: reverse() function"){

    val list = List(1,2,3,4,5,6,7,8,9)
    val reversed = List.reverse(list)
    assertResult(List(9,8,7,6,5,4,3,2,1))(reversed)
    println(reversed)
  }

  test("Test length"){

    val list = List(1,2,3,4,5,6,7,8)
    val length = List.length(list)
    assertResult(8)(length)
    println(length)
  }

  test("Testing 3.11: product with foldLeft() "){

    val list:List[Double] = List(1,2,3,4)
    val result = List.product3(list)
    assertResult(24)(result)
    println(result)
  }

  test("Testing 3.11: sum with foldLeft()"){

    val list = List(1,2,3,4)
    val result = List.sum3(list)
    assertResult(10)(result)
    println(result)
  }

  test("Testing 3.10 with sum"){

    val list = List(1,2,3,4,5)
    val result = List.foldLeft(list,0)(_+_)

    assertResult(15)(result)
   println(result)
  }

  test("Testing 3.10"){

    val list = List(1,2,3,4,5,6,7,8)
    val result = List.foldLeft(list,Nil:List[Int])((b,a)=> Cons(a,b))
    assertResult(List(8,7,6,5,4,3,2,1))(result)
    println(result)
  }

  test("Testing 3.8"){

    val list = List(1,2,3,4,5,6,7,8)
    val result = List.foldRight(list,Nil:List[Int])(Cons(_,_))
    assertResult(list)(result)
    println(result)
  }

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

  test("Test sum2()"){

    val nums:List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
    //println(nums)
    val result = List.sum2(nums)
    println(result)
    assertResult(6)(result)
  }

  test("Test sum()"){

    val nums:List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
    //println(nums)
    val result = List.sum(nums)
    println(result)
    assertResult(6)(result)
  }

  test("Test product2() "){

    val nums:List[Double] = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0,Nil))))
    val result = List.product2(nums)
    println(result)
    assertResult(24)(result)
  }

  test("Test product() "){

    val nums:List[Double] = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0,Nil))))
    val result = List.product(nums)
    println(result)
    assertResult(24)(result)
  }
}
