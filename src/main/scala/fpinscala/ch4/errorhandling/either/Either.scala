package fpinscala.ch4.errorhandling.either

import scala.{Either => _, Option => _, _}


sealed trait Either[+E, +A]{

  def map[B](f:A=>B): Either[E, B] = this match {
    case Right(s) => Right(f(s))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f:A => Either[EE,B]):Either[EE,B] = this match {
    case Left(e)=> Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f:(A,B) => C): Either[EE,C]
                                      = for {a <- this; b1 <- b} yield f(a,b1)

}
case class Left[+E](value:E) extends Either[E, Nothing]
case class Right[+A](value:A) extends Either[Nothing, A]

object Either {

  def sequence_1[E,A](seq:List[Either[E,A]]):Either[E,List[A]] = seq match {
    case Nil => Right(Nil)
    case x :: xs => x flatMap(h => sequence_1(xs) map(h :: _))
  }

  def sequence_2[E,A](seq:List[Either[E,A]]):Either[E,List[A]]
        = seq.foldRight[Either[E,List[A]]](Right(Nil))((x:Either[E,A], y:Either[E,List[A]]) => x.map2(y)(_ :: _))

  def traverse[E,A,B](seq:List[A])(f:A=>Either[E,B]):Either[E,List[B]]
      = seq.foldRight[Either[E,List[B]]](Right(Nil))((a,b) => f(a).map2(b)(_ :: _) )

  def traverse_1[E,A,B](seq:List[A])(f:A => Either[E,B]):Either[E,List[B]] = seq match {
    case Nil => Right(Nil)
    case x :: xs => (f(x) map2 traverse_1(xs)(f))(_ :: _)
  }

}

