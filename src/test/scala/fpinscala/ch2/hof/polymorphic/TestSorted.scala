package fpinscala.ch2.hof.polymorphic

import org.scalatest.funsuite.AnyFunSuite
import fpinscala.ch2.hof.polymorphic.Sorted._

class TestSorted extends AnyFunSuite{

  test("Test isSorted (by array length) with sorted array"){

    val arr = Array("I", "III", "IIII", "IIIII", "IIIIII")
    val sorted = isSorted(arr, (s1:String,s2:String)=> s1.length< s2.length)
    assert(sorted)
  }

  test("Test isSorted (by array length) with unsorted array"){

    val arr = Array("IIII","I", "III",  "IIIII", "IIIIII")
    val sorted = isSorted(arr, (s1:String,s2:String)=> s1.length< s2.length)
    assert(!sorted)
  }

}
