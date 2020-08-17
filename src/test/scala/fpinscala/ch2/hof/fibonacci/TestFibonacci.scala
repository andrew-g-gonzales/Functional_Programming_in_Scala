package fpinscala.ch2.hof.fibonacci

import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.fibonacci.Fibonacci._

class TestFibonacci extends AnyFunSuite{

  test("test non tail-call recursive factorial "){

    val result = fibonacci_1(4)
    assert(result == 3)
  }


}
