package fpinscala.ch4.errorhandling.either

import org.scalatest.funsuite.AnyFunSuite

import org.scalatest.funsuite.AnyFunSuite
import scala.{Either => _, Option => _, _}
import fpinscala.ch4.errorhandling.either.Either
import fpinscala.ch4.errorhandling.either.Left
import fpinscala.ch4.errorhandling.either.Right

class TestEither extends AnyFunSuite {

  def toInt(str:String): Either[Exception,Int]
      = try Right(str.toInt) catch {case e:Exception => Left(e)}

  //https://stackoverflow.com/questions/57986585/convert-a-number-to-a-string-representation-in-scala-without-using-tostring
  def int2str(num :Int) :Either[Exception,String] = {
    try {
      val ll = LazyList.iterate(math.abs(num))(_ / 10)
      val res = (ll.head +: ll.tail.takeWhile(_ > 0)).map(n => (n % 10 + 48).toChar)
        .reverse
        .mkString
      val result = if (num < 0) "-" + res else res
      Right(result)
    }catch{ case e:Exception => Left(e) }
  }

  def mean(xs:IndexedSeq[Double]): Either[String,Double]
      = if(xs.isEmpty) Left("empty list") else Right(xs.sum/xs.length)

  def safeDiv(x:Int, y:Int): Either[Exception, Int]
                    = try Right(x/y)catch {case e:Exception => Left(e)}

  test("Testing 4.6: Implement map2 on Either that operates on Right value"){

    val eitherResult:Either[Exception,Either[Exception,Int]] =  toInt("20").map2(toInt("5"))(safeDiv)
    assertResult(Right(Right(4)))(eitherResult)
    println(eitherResult)
  }

  test("Testing 4.6: Implement orElse on Either that operates on Right value"){

    val strNoInt = "5A5"
    val bak2Str = toInt(strNoInt).orElse(Right(0))
    assertResult(Right(0))(bak2Str)
    println(bak2Str)
  }

  test("Testing 4.6: Implement flatMap on Either that operates on Right value"){

    val strInt = "55"
    val bak2Str = toInt(strInt).flatMap(int2str)
    assertResult(Right("55"))(bak2Str)
    println(bak2Str)
  }

  test("Testing 4.6: Implement map on Either that operates on Right value"){

    val seq = IndexedSeq(5.75, 11.90)
    val eitherMeanStr = mean(seq).map(_.toString)
    assertResult(Right("8.825"))(eitherMeanStr)
    println(eitherMeanStr)
  }

}
