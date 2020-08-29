package fpinscala.ch3.datastructures.list

trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {

  def foldRight[A,B](ds:List[A],z:B)(f:(A,B)=>B):B = ds match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs,z)(f))
  }

  def sum2(list:List[Int]):Int = foldRight(list,0)(_+_)

  def product2(list:List[Double]):Double = foldRight(list,1.0)((x,y)=> if(x == 0.0 || y ==0.0)0.0 else x*y)

  def init[A](ds:List[A]):List[A] = ds match {
    case Cons(_, Nil) | Nil => Nil
    case Cons(x,xs) => Cons(x,init(xs))
  }

  def append[A](a1:List[A], a2:List[A]):List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t,a2))
    case Cons(h, Nil)=> Cons(h, append(a2,Nil))
  }

  def dropWhile[A](ds:List[A])(drop:A => Boolean):List[A] = ds match{
    case Cons(x,xs) if drop(x) => dropWhile(xs)(drop)
    case _ =>ds
  }

  def dropIf[A](ds:List[A], drop:A=>Boolean):List[A] = ds match {
    case Cons(x,xs) if drop(x) => dropIf(xs,drop)
    case Cons(x,xs)  => Cons(x,dropIf(xs,drop))
    case Nil => Nil
  }

  def drop[A](ds:List[A], n:Int):List[A] = {
    if( n ==0){ds}
    else{
      ds match {
        case Cons(_,xs)=> drop(xs,n-1)
        case Cons(_,Nil) | Nil => Nil
      }
    }
  }

  def setHead[A](ds:List[A], a:A):List[A] = ds match {
    case Nil | Cons(_,Nil) => Cons(a, Nil)
    case Cons(x,xs) => Cons(a,xs)
  }

  def tail[A](ds:List[A]) = ds match{
    case Nil => Nil
    case Cons(x,Nil)=> x
    case Cons(_,xs)=>xs
  }

  def product(ds:List[Double]):Double = ds match {
    case Nil => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def sum(ints:List[Int]):Int = ints match {
    case Nil =>0
    case Cons(x,xs) => x+sum(xs)
  }

  def apply[A](as:A*):List[A] = if(as.isEmpty)Nil else Cons(as.head,apply(as.tail:_*))
}
