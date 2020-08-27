package fpinscala.ch2.hof.fibonacci

import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.fibonacci.Fibonacci._
import fpinscala.ch2.hof.Big


class TestFibonacci extends AnyFunSuite{

  test("test non tail-call recursive factorial "){

    val result = fibonacci_2(7)
    assert(result == 13)
  }

  test("Test tail call recursive program"){

    val result = fibonacci_2(7)
    assert(result == 13)
  }


}
