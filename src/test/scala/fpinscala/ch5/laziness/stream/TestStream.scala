package fpinscala.ch5.laziness.stream

import scala.{Stream => _, _}
import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch5.laziness.stream._

class TestStream extends AnyFunSuite {

  test("Testing 5.10:  Write a function fibs that generates the infinite stream of Fibonacci numbers: " +
    "0, 1, 1, 2, 3, 5, 8, and so on."){

    val fibs = Stream.fibs.take(5).toList3
    assertResult(List(0,1,1,2,3))(fibs)
    println(fibs)
  }

  test("Testing 5.12:  Write the 'fibs' function using 'unfold'"){

    val fibs = Stream.fibs2.take(5).toList3
    assertResult(List(0,1,1,2,3))(fibs)
    println(fibs)
  }

  test("Testing 5.12: Write the 'constant' function using 'unfold'"){

    val threeAs = Stream.constantViaUnfold("A").take(3).toList3
    assertResult(List("A", "A", "A"))(threeAs)
    println(threeAs)
  }

  test("Testing 5.12: Write the 'from' function using 'unfold'"){

    val to10 = Stream.fromViaUnfold(1).take(10).toList3
    assertResult(List(1,2,3,4,5,6,7,8,9,10))(to10)
    println(to10)
  }

  test("Testing 5.9: Write a function that generates an infinite stream of integers, starting from n, then n " +
    "+ 1, n + 2, and so on"){

    val to10 = Stream.from(1).take(10).toList3
    assertResult(List(1,2,3,4,5,6,7,8,9,10))(to10)
    println(to10)
  }

  test("Testing 5.12: the 'ones' function in terms of 'unfold'"){

    val five1s = Stream.onesViaUnfold.take(5)
    assertResult(List(1,1,1,1,1))(five1s.toList3)
    println(five1s)

    val evenExists = Stream.onesViaUnfold.map(_+1).exists(_%2 == 0)
    assertResult(true)(evenExists)

    val noOnes = Stream.onesViaUnfold.forAll2(_ != 1)
    println(noOnes)
  }

  test("Testing the ones function"){

    val five1s = Stream.ones.take(5)
    assertResult(List(1,1,1,1,1))(five1s.toList3)
    println(five1s)

    val evenExists = Stream.ones.map(_+1).exists(_%2 == 0)
    assertResult(true)(evenExists)

    val noOnes = Stream.ones.forAll2(_ != 1)
    println(noOnes)
  }

  test("Testing 5.7: Implement append using foldRight"){

    val stream = Stream(1,2,3,4,5)
    val appended = stream.append(Stream(6,7,8,9,10)).toList3
    assertResult(List(1,2,3,4,5,6,7,8,9,10))(appended)
    println(appended)
  }

  test("Testing 5.7: Implement filter using foldRight"){

    val stream = Stream(8,6,7,5,3,0,9)
    val lt5 =stream.filter(_<5).toList3
    assertResult(List(3,0))(lt5)
    println(lt5)
  }

  test("Testing 5.7: Implement map using foldRight"){

    val stream = Stream(1,2,3,4,5)
    val doubled = stream.map(_*2).toList3
    assertResult(List(2,4,6,8,10))(doubled)
    println(doubled)
  }

  test("Testing 5.13: Implement map using unfold"){

    val stream = Stream(1,2,3,4,5)
    val doubled = stream.map(_*2).toList3
    assertResult(List(2,4,6,8,10))(doubled)
    println(doubled)
  }

  test("Testing 5.6: Implement headOption using foldRight."){

    val stream = Stream(1,2,3)
    val optHead = stream.headOption
    assertResult(Some(1))(optHead)
    println(optHead)
  }

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

  test("Testing 5.13: Using unfold to implement zipAll"){


  }

  test("Testing 5.13: Implement a zipAll function using unfold." +
    "The zipAll function should continue the traversal as long as either stream" +
    "has more elements—it uses Option to indicate whether each stream has been exhausted."){

    val stream1 = Stream("A","B","C","D")
    val stream2 = Stream(1, 2, 3)
    val zipped = stream1.zipAll(stream2).toList2

    assertResult(List((Some("A"),Some(1)), (Some("B"),Some(2)), (Some("C"),Some(3)), (Some("D"),None)))(zipped)
    println(zipped)
  }

  test("Testing 5.14: Implement startsWith using functions you’ve written. It should check if one" +
    "Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2) would be true"){

    val stream1 = Stream(1,2,3)
    val stream2 = Stream(1,2)
    assert(stream1.startsWith(stream2))
  }

  test("Test the hasSubsequence"){

    val stream = Stream(1,2,3,4,5,6,7,8,9)
    val subsequence = Stream(6,7,8)
    assert(stream.hasSubsequence(subsequence))
  }

  test("Testing 5.15: Generalize tails to the function scanRight, " +
    "which is like a foldRight that returns a stream of the intermediate results."){

    val stream = Stream(1,2,3)
    val result = stream.scanRight(0)(_+_).toList2
    assertResult(List(6,5,3,0))(result)
    println(result)
  }

  test("Testing 5.15: Implement tails using unfold. For a given Stream, " +
    "tails returns the Stream of suffixes of the input sequence, starting with " +
    "the original Stream. For example, given Stream(1,2,3), " +
    "it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())."){

    val stream = Stream(1,2,3,4,5,6,7,8,9,10)
    println(stream.tails.toList2.map(_.toList2))
  }

  test("Testing 5.13: Using unfold to implement zipWithAll"){
    val stream1 = Stream("A","B","C")
    val stream2 = Stream(1, 2, 3)

    val zipped = stream1.zipWithAll(stream2)(pairUp).toList3
    assertResult(List("A1", "B2", "C3"))(zipped)
    println(zipped)
  }

  def pairUp[A,B,C](a:Option[A], b:Option[B]):String = (a,b) match {
    case (None,None)=> ""
    case (Some(x),None) => s"$x"
    case (None,Some(y)) => s"$y"
    case (Some(x), Some(y)) => s"$x$y"
  }

  test("Testing 5.13: Using unfold to implement zipWith"){

    val stream1 = Stream("A", "B", "C")
    val stream2 = Stream(1, 2, 3)
    val zipped = stream1.zipWith(stream2)((a,b)=> s"$a$b").toList3
    assertResult(List("A1", "B2", "C3"))(zipped)
    println(zipped)
  }

  test("Testing 5.13: Implement takeWhile in terms of unfold"){

    val stream = Stream(8,6,7,5,3,0,9)
    val gT4 = stream.takeWhileViaUnfold(_ > 4).toList3
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

  test("Testing 5.13: Implement take using unfold"){

    val stream = Stream(1,2,3,4,5,6,7,8,9,10)
    val first5 = stream.takeViaUnfold(5).toList2
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
