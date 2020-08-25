package fpinscala.ch2.hof

import org.scalatest.funsuite.AnyFunSuite

class TestHOFs extends AnyFunSuite{

  test("test string HOF"){

    val f1 = (s:String) => s+s

    val f2 = new Function1[String,String]{
      override def apply(s:String):String = s+s
    }

    assert(f1("foo") == f2("foo"))
  }

}
