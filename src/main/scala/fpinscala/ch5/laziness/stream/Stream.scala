package fpinscala.ch5.laziness.stream

import fpinscala.ch5.laziness.stream.Stream.cons
import Stream._
import fpinscala.ch2.hof.Big
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

  def append[B>:A](tl: =>Stream[B]):Stream[B]
                      = foldRight(tl)((h,t) => cons(h,t))

  def filter(p:A=>Boolean):Stream[A] = foldRight(Empty:Stream[A])((a,b) => if(p(a)) cons(a,b) else b)

  def map[B](f:A=>B):Stream[B] = foldRight(Empty:Stream[B])((a,b) => cons(f(a),b))

  def mapViaUnfold[B](f:A=>B)
  = unfold(this){
    case Cons(h,t) => Some((f(h()),t()))
    case _ => None
  }

  def forAll2(p:A=>Boolean): Boolean = foldRight(true)((a,b)=> p(a) && b)

  def exists2(p:A=>Boolean): Boolean = foldRight(false)((a,b)=>p(a) || b)

  def exists(p:A=>Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

  def headOptionUsingFoldRight: Option[A]
          = foldRight(None: Option[A]) ((a,_) => Option(a))

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

  def takeViaUnfold(n:Int)
  = unfold(this){
    case Cons(h,t) if n > 1 => Some((h(),t().take(n-1)))
    case Cons(h,_) if n ==1 => Some((h(),empty))
    case _ => None
  }

  def drop(n:Int):Stream[A] = this match {
    case Cons(_,t) if n > 1 => t().drop(n-1)
    case Cons(_,t) if n ==1 => t()
    case _ => empty
  }

  def takeWhileUsingFoldRight(f:A=>Boolean): Stream[A]
  = foldRight(empty[A])((a,b) => if (f(a)) cons(a,b) else empty)

  def takeWhileViaUnfold(p:A=>Boolean):Stream[A] =
    unfold(this){
      case Cons(h,t) if p(h()) => Some((h(),t().takeWhileViaUnfold(p)))
      case _ => None
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

  def fibs:Stream[Int] = {

    def go(f0:Int, f1:Int):Stream[Int] = {
      cons(f0,go(f1,f0+f1))
    }

    go(0,1)
  }

  def cons[A](hd: =>A, tl: =>Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def ones:Stream[Int] = cons(1,ones)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  def fibs2:Stream[Int] = unfold((0,1)){case (f0,f1)=> Some(f0,(f1,f0+f1))}

  def fromViaUnfold(n:Int):Stream[Int] = unfold(n)(n => Some((n,n+1)))

  def onesViaUnfold:Stream[Int] = unfold(1)(_=> Some((1,1)))

  def constantViaUnfold[A](a:A):Stream[A] = unfold(a)(_ => Some((a,a)))

  def from(n:Int):Stream[Int] = cons(n,from(n+1))
}
