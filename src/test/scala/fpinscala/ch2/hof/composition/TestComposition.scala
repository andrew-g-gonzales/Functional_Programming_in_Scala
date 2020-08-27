package fpinscala.ch2.hof.composition

import fpinscala.ch2.hof.composition.Composition._
import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.fibonacci.Fibonacci._
import fpinscala.ch2.hof.Big

class TestComposition extends AnyFunSuite{

  val square = (x:Double)=>x.toInt*x.toInt
  val toStr = (i:Int) => s"$i"

  test("Test compose function"){

    val squareStr = compose(square,toStr )
    val result = squareStr(8)
    assertResult("64")(result)
    println(result)

  }

  test("Test Function1 compose function"){

    val squareStr = toStr compose square

    val result = squareStr(8)
    assertResult("64")(result)
    println(result)
  }
}
