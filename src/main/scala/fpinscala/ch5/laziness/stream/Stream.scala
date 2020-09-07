package fpinscala.ch5.laziness.stream

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
