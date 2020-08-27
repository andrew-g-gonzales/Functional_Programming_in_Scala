package fpinscala.ch2.hof.currying

import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.currying.Currying._

class TestCurrying extends AnyFunSuite{

  test("Test Currying 1"){

    val product = curry[Int,Int,String]((a,b)=>(String.valueOf(a*b)))
    val result = product(5)(10)
    println(result)
    assertResult(50)(result)
  }

  test("Test Curry Proof"){

    def f(a:Int, b:Int) = a+b
    def g(a:Int)(b:Int) = a+b

    val first =  curry(f)(1)(1) == f(1,1)
    assert(first)
    val second =  curry(f)(1)(1) == g(1)(1)
    assert(second)
  }

  test("testing currying with example from ch. 6 of 'Scala Functional Patterns book'"){

   val aFunc = higher(pred)
    assert(!aFunc(20))
  }

  test("Test uncurrying"){

    val sum = (a:Int)=>(b:Int)=>a+b
    val uncurried = uncurry(sum)
    val sum1 = sum(1)(2)
    val sum2 =uncurried(1,2)

    assert(sum1 == sum2)
  }



}
