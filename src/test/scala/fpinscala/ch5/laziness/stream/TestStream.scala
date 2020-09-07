package fpinscala.ch5.laziness.stream

import scala.{Stream => _, _}
import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch5.laziness.stream._

class TestStream extends AnyFunSuite {

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
