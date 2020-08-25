package fpinscala.ch2.hof.partial

import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.partial.Partial._

class TestPartial extends AnyFunSuite{

  test("Partially Applied [Int,Int,String]"){

    val addAndStringify:Int=>String = partial1[Int,Int,String](1,(a,b)=>(String.valueOf(a+b)))
    val stringResult = addAndStringify(20)
    assertResult("21")(stringResult)
    println(stringResult)
  }

  test("Partially Applied [String,String,Boolean]"){

    val check:String=>Boolean = partial1[String,String,Boolean]("*123*",contains)
    val doesItContain =  check("abc1234def")
    assert(doesItContain)
    println(doesItContain)
  }

  def contains(str:String, str2:String):Boolean= str.matches(".*\\d.*")
}
