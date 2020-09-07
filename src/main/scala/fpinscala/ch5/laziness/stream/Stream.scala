package fpinscala.ch5.laziness.stream

import fpinscala.ch5.laziness.stream.Stream.cons

import Stream._
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,_) => Some(h())
  }

  def toList: List[A] = {

    def go(stream: Stream[A]):List[A]  = stream match {
      case Empty => Nil
      case Cons(h,t) => h() :: go(t())
    }

    go(this)
  }

  def toList2: List[A] = this match {
    case Cons(h,t) => h() :: t().toList2
    case _ => List()
  }

  def take(n:Int):Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 => cons(h(),empty)
    case _ => empty
  }

  def drop(n:Int):Stream[A] = this match {
    case Cons(_,t) if n > 1 => t().drop(n-1)
    case Cons(_,t) if n ==1 => t()
    case _ => empty
  }

  def takeWhile(p:A=>Boolean):Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def toList3: List[A] = {

    def go(stream: Stream[A], acc:List[A]):List[A]  = stream match {
      case Empty => acc
      case Cons(h,t) => go(t(), h() :: acc)
    }

    go(this, Nil).reverse
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h:()=>A, t:()=>Stream[A]) extends Stream[A]

object Stream{

  def cons[A](hd: =>A, tl: =>Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))



}
