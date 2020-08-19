package fpinscala.ch2.hof.polymorphic

import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.polymorphic.Find._

class TestFind  extends AnyFunSuite{

  test("Test Polymorphic Find Function with Strings"){
    val arr  = Array("Godel", "Escher", "Bach")
    val results:Array[Int] =arr.map(s=> findFirst(arr,(str:String) => str == s))
    assertResult(Array(0,1,2))(results)
    println(results.mkString(","))
  }

  test("Test Polymorphic Find Function not found case with Strings"){
    val arr  = Array("Godel", "Escher", "Bach")
    val result = findFirst(arr,(s:String)=> s == "Picasso" )
    assertResult(-1)(result)
  }

  test("Test Monomorphic Function Find all"){
    val arr  = Array("Godel", "Escher", "Bach")
    val results:Array[Int] =arr.map(s=> findFirstMonomorphic(arr,s))
    assertResult(Array(0,1,2))(results)
    println(results.mkString(","))
  }

  test("Test Monomorphic Function not found"){
    val arr  = Array("Godel", "Escher", "Bach")
    val result = findFirstMonomorphic(arr,"Picasso")
    assertResult(-1)(result)
  }

}
