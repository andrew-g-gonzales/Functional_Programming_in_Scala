package fpinscala.ch2.hof.factorial

import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.factorial.Factorial._

class TestFactorial extends AnyFunSuite{

  test("test recursive factorial with tail call optimization"){

    val n = factorial(8)
    println("factorial: "+n)
    assert(n== 40320)
  }

  test("test recursive factorial without tail call optimization"){

    val n = factorial2(8)
    println("factorial2: "+n)
    assert(n == 40320)
  }


}
