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

  test("Listing 4.4"){

    case class Person(name:Name, age:Age)
    sealed case class Name(value:String)
    sealed case class Age(value:Int)

    def mkName(name:String):Either[String,Name] = name match{
      case nm if(nm == null || nm.isEmpty) => Left("name is empty")
      case nm => Right(Name(nm))
    }

    def mkAge(age:Int):Either[String,Age] = age match {
      case a if(a < 0) => Left("age is less than zero")
      case a => Right(Age(a))
    }

    def mkPerson(name:String,age:Int)
          = mkName(name).map2(mkAge(age))(Person)

    val eitherPerson = mkPerson("John Doe", 100)
    assertResult(Right(Person(Name("John Doe"), Age(100))))(eitherPerson)
    println(eitherPerson)
  }

  test("Testing 4.7: Implement traverse with pattern-matching/recursion"){

    val numList = List(8,6,7,5,3,0)
    val eitherList = Either.traverse_1(numList)(int2str)
    assertResult(Right(numList.map(_.toString)))(eitherList)
    println(eitherList)
  }

  test("Testing 4.7: Implement traverse with foldRight"){

    val numList = List(8,6,7,5,3,0)
    val eitherList = Either.traverse(numList)(int2str)
    assertResult(Right(numList.map(_.toString)))(eitherList)
    println(eitherList)
  }

  test("Testing 4.7: Implement sequence with foldRight"){

    val numList = List(8,6,7,5,3,0)
    val listOfEitherNums:List[Either[Exception,String]] = List(8,6,7,5,3,0).map(int2str)
    val eitherList:Either[Exception,List[String]] = Either.sequence_2(listOfEitherNums)
    assertResult(Right(numList.map(_.toString)))(eitherList)
    println(eitherList)
  }

  test("Testing 4.7: Implement sequence with pattern-matching/recursion"){

    val numList = List(8,6,7,5,3,0)
    val listOfEitherNums:List[Either[Exception,String]] = List(8,6,7,5,3,0).map(int2str)
    val eitherList:Either[Exception,List[String]] = Either.sequence_1(listOfEitherNums)
    assertResult(Right(numList.map(_.toString)))(eitherList)
    println(eitherList)
  }

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
