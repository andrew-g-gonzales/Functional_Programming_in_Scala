package fpinscala.ch2.hof

import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.Formatter._
import fpinscala.ch2.hof.fibonacci.Fibonacci._
import fpinscala.ch2.hof.factorial.Factorial._

class TestFormatter extends AnyFunSuite{

  test("Testing Fibonacci"){
    val msg = formatResult("Fibonacci", 14, fibonacci_1)
    assertResult("The Fibonacci of 14 is 377")(msg)
    println(msg)
  }

  test("Testing Factorial"){
    val msg = formatResult("Factorial", 99, factorial)
    assertResult("The Factorial of 99 is 933262154439441526816992388562667004907159682643816214685929638952175999932299156089414639761565182862536979208272237582511852109168640000000000000000000000")(msg)
    println(msg)
  }
}
