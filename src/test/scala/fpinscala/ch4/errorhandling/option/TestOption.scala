package fpinscala.ch4.errorhandling.option

import scala.{Either => _, Option => _, _}
import fpinscala.ch4.errorhandling.option.Option
import fpinscala.ch4.errorhandling.option.Some
import fpinscala.ch4.errorhandling.option.None
import org.scalatest.funsuite.AnyFunSuite

class TestOption extends AnyFunSuite {

  test("Testing 4.5 Some result:   Implement this function. It’s straightforward to do using map and sequence, but try" +
    "for a more efficient implementation that only looks at the list once. In fact, implement sequence in terms of " +
    "traverse."){

    val list = List("1",  "2",  "3")

    val traversed:Option[List[Int]] = Option.traverse(list)(a => Option.Try(Integer.valueOf(a)))
    val traversed2:Option[List[Int]] = Option.traverse_2(list)(a => Option.Try(Integer.valueOf(a)))
    assertResult(Some(List(1,2,3)))(traversed)
    assertResult(Some(List(1,2,3)))(traversed2)
    println(traversed)
    println(traversed2)
  }

  test("Testing 4.5 None result:   Implement this function. It’s straightforward to do using map and sequence, but try" +
    "for a more efficient implementation that only looks at the list once. In fact, implement sequence in terms of " +
    "traverse."){

    val list = List("1", "A",  "2",  "3")

    val traversed:Option[List[Int]] = Option.traverse(list)(a => Option.Try(Integer.valueOf(a)))
    assertResult(None)(traversed)
    println(traversed)
  }

  test("Testing 4.4:  Write a function sequence that combines a list of Options into " +
    "one Option containing a list of all the Some values in the original list. If the original " +
    "list contains None even once, the result of the function should be None; otherwise the " +
    "result should be Some with a list of all the values."){

    val listOfOptions = List(Some(8), Some(6), Some(7), Some(5), Some(3),Some(0),Some(9))
    val optList:Option[List[Int]] = Option.sequence_2(listOfOptions)
    assertResult(Some(List(8,6,7,5,3,0,9)))(optList)
    println(optList)
  }

  test("Testing 4.2: Implement the variance function in terms of flatMap. If the mean of a sequence is m," +
    "the variance is the mean of math.pow(x - m, 2) for each element x in the sequence."){

    val list = Seq(8.0,6.0,7.0,5.0,3.0,0.0,9.0)
    val variance = Option.variance(list)
    assertResult(Some(8.244897959183673))(variance)
    println(variance)
  }

}
