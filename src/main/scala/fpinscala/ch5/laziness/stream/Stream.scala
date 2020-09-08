package fpinscala.ch5.laziness.stream

import fpinscala.ch5.laziness.stream.Stream.cons

import Stream._
sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f:(A, => B)=>B):B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p:A=>Boolean): Boolean = this match {
    case Cons(h,t) if p(h()) => t().forAll(p)
    case Empty => true
    case _ => false
  }



  def forAll2(p:A=>Boolean): Boolean = foldRight(true)((a,b)=> p(a) && b)

  def exists2(p:A=>Boolean): Boolean = foldRight(false)((a,b)=>p(a) || b)

  def exists(p:A=>Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

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

  def takeWhileUsingFoldRight(f:A=>Boolean)
     = foldRight(empty[A])((a,b) => if (f(a)) cons(a,b) else empty)

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
