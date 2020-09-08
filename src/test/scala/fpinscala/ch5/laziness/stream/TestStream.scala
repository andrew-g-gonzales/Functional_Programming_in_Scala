package fpinscala.ch5.laziness.stream

import scala.{Stream => _, _}
import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch5.laziness.stream._

class TestStream extends AnyFunSuite {

  test("Testing 5.4 for true case with foldRight: Implement forAll, which checks that all elements in the Stream match a given predicate." +
    "Your implementation should terminate the traversal as soon as it encounters a nonmatching value."){

    val stream = Stream(6,7,8,9,10,11)
    val allTrue = stream.forAll2(_>5)
    assert(allTrue)
    println(allTrue)
  }

  test("Testing 5.4 for true case: Implement forAll, which checks that all elements in the Stream match a given predicate." +
    "Your implementation should terminate the traversal as soon as it encounters a nonmatching value."){

    val stream = Stream(6,7,8,9,10,11)
    val allTrue = stream.forAll(_>5)
    assert(allTrue)
    println(allTrue)
  }

  test("Testing 5.4 for false case with foldRight: Implement forAll, which checks that all elements in the Stream match a given predicate." +
    "Your implementation should terminate the traversal as soon as it encounters a nonmatching value."){

    val stream = Stream(6,7,8,9,10,11,5,12)
    val allTrue = stream.forAll2(_>5)
    assertResult(false)(allTrue)
    println(allTrue)
  }

  test("Testing 5.4 for false case: Implement forAll, which checks that all elements in the Stream match a given predicate." +
    "Your implementation should terminate the traversal as soon as it encounters a nonmatching value."){

    val stream = Stream(6,7,8,9,10,11,5,12)
    val allTrue = stream.forAll(_>5)
    assertResult(false)(allTrue)
    println(allTrue)
  }

  test("Testing exists"){

    val stream = Stream(8,6,7,5,3,0,9)
    val exists = stream.exists(_ == 5)
    assert(exists)
    println(exists)
  }

  test("Testing exists implemented with foldRight"){

    val stream = Stream(8,6,7,5,3,0,9)
    val exists = stream.exists2(_ == 5)
    assert(exists)
    println(exists)
  }

  test("Testing 5.3 via foldRight: Write the function takeWhile for returning all starting elements of a Stream that " +
    "match the given predicate."){

    val stream = Stream(8,6,7,5,3,0,9)
    val gT4 = stream.takeWhileUsingFoldRight(_ > 4).toList3
    assertResult(List(8,6,7,5))(gT4)
    println(gT4)
  }

  test("Testing 5.3: Write the function takeWhile for returning all starting elements of a Stream that " +
    "match the given predicate."){

    val stream = Stream(8,6,7,5,3,0,9)
    val gT4 = stream.takeWhile(_ > 4).toList3
    assertResult(List(8,6,7,5))(gT4)
    println(gT4)
  }

  test("Testing 5.2: drop(n) for skipping the first n elements of a Stream"){

    val stream = Stream(1,2,3,4,5,6,7,8,9,10)
    val last5 = stream.drop(5).toList3
    assertResult(List(6,7,8,9,10))(last5)
    println(last5)
  }

  test("Testing 5.2: Write the function take(n) for returning the first n elements of a Stream"){

    val stream = Stream(1,2,3,4,5,6,7,8,9,10)
    val first5 = stream.take(5).toList2
    assertResult(Stream(1,2,3,4,5).toList2)(first5)
    println(first5)
  }

  test("Testing 5.1 1: Write a function to convert a Stream to a List, which will force its evaluation and let" +
    "you look at it in the REPL. You can convert to the regular List type in the standard library. " +
    "You can place this and other functions that operate on a Stream inside the Stream trait."){

    val stream = Stream(8,6,7,5,3,0,9)
    val list = stream.toList
    assertResult(List(8,6,7,5,3,0,9))(list)
    println(list)
  }

  test("Testing 5.1 2: Write a function to convert a Stream to a List, which will force its evaluation and let" +
    "you look at it in the REPL. You can convert to the regular List type in the standard library. " +
    "You can place this and other functions that operate on a Stream inside the Stream trait."){

    val stream = Stream(8,6,7,5,3,0,9)
    val list = stream.toList2
    assertResult(List(8,6,7,5,3,0,9))(list)
    println(list)
  }

  test("Testing 5.1 3: Write a function to convert a Stream to a List, which will force its evaluation and let" +
    "you look at it in the REPL. You can convert to the regular List type in the standard library. " +
    "You can place this and other functions that operate on a Stream inside the Stream trait."){

    val stream = Stream(8,6,7,5,3,0,9)
    val list = stream.toList3
    assertResult(List(8,6,7,5,3,0,9))(list)
    println(list)
  }

}
