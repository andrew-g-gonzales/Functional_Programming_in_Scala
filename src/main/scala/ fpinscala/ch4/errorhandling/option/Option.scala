package  fpinscala.ch4.errorhandling.option

import scala.{::, Either => _, Option => _, _}

sealed trait Option[+A]{

  def map[B](f:A=>B):Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f:A=>Option[B]):Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B):B = this match {
    case Some(a)=> a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]):Option[B] = this map (Some(_)) getOrElse ob

  def filter(f:A => Boolean): Option[A] = this match {
    case Some(a) if(f(a)) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def variance(xs:Seq[Double]):Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x-m,2))))

  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty){
      None
    }else{
      val mean:Double =  xs.sum/xs.length
      Some(mean)
    }
  }

  def map2[A,B,C](a:Option[A], b:Option[B])(f:(A,B)=>C) =
                         a flatMap(aa => b map (bb => f(aa,bb)))

  def map2_for_comprehension[A,B,C](a:Option[A], b:Option[B])(f:(A,B)=>C):Option[C] =
    for{
      aa <- a
      bb <- b
    } yield(f(aa,bb))

  def sequence_1[A](seq:List[Option[A]]):Option[List[A]] = seq match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap(h => sequence_1(xs) map (h :: _))
  }

  def sequence_2[A](seq:List[Option[A]]):Option[List[A]] =
                 seq.foldRight[Option[List[A]]](Some(Nil))((x:Option[A],y:Option[List[A]])=> map2(x,y)(_ :: _))

  def traverse[A,B](list:List[A])(f:A=>Option[B]):Option[List[B]] =
                                        list.foldRight[Option[List[B]]](Some(Nil))((x,xs)=> map2(f(x),xs)(_::_))

  def traverse_2[A,B](list:List[A])(f:A=>Option[B]):Option[List[B]] =
    list.foldRight[Option[List[B]]](Some(Nil))((x,xs)=> map2_for_comprehension(f(x),xs)(_::_))

  def Try[A](a: => A) :Option[A] = {
    try Some(a)
    catch {case e :Exception=> None}
  }
}
