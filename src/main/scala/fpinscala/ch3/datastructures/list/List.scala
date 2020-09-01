package fpinscala.ch3.datastructures.list

trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {

  def addPairWise[A](list1:List[Int], list2:List[Int]):List[Int] = (list1,list2) match {
    case (Nil,_) =>Nil
    case (_,Nil) =>Nil
    case (Cons(x,xs), Cons(y,ys)) => Cons(x+y, addPairWise(xs,ys))
  }

  def map2[A,B](list:List[A])(f:A=>B):List[B] = foldRight(list,Nil:List[B])((b,a)=> Cons(f(b),a))

  def map[A,B](list:List[A])(f:A=>B):List[B] = list match {
    case Cons(x,xs) => Cons(f(x), map(xs)(f))
    case Nil => Nil
  }

    def stringifyDoubleList(list:List[Double]):List[String] =list match {
      case Cons(x,xs) => Cons(String.valueOf(x), stringifyDoubleList(xs))
      case Nil => Nil
    }

    def add1(list:List[Int]):List[Int] = list match {
      case Cons(x,xs) => Cons(x+1, add1(xs))
      case Nil=>Nil
    }

  def filterViaFlatMap[A](list:List[A])(f:A=>Boolean):List[A] = flatMap(list)(a=> if(f(a)) List(a) else Nil)

    def flatMap[A,B](list:List[A])(f:A=>List[B]):List[B] = flatten(map(list)(f))

  def flatten[A](listOfLists:List[List[A]]):List[A] = foldLeft(listOfLists, Nil:List[A])((a,b)=> List.append2(a,b))

  def append2[A](list1:List[A], list2:List[A]) = foldLeft(reverse(list1),list2)((b,a)=>Cons(a,b))

  def append1[A](list1:List[A], list2:List[A]) = foldRight(list1,list2)(Cons(_,_))

  def reverse[A](list:List[A]) = foldLeft(list,Nil:List[A])((b,a)=> Cons(a,b))

  @annotation.tailrec
  def foldLeft[A,B](ds:List[A],z:B)(f:(B,A)=>B):B = ds match {
      case Nil =>z
      case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
  }

  def length[A](ds:List[A]) = foldRight(ds,0)((_,y)=>y+1)

  def foldRight[A,B](ds:List[A],z:B)(f:(A,B)=>B):B = ds match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs,z)(f))
  }

  def sum3(list:List[Int]) = foldLeft(list,0)(_+_)

  def product3(list:List[Double]) = foldLeft(list,1.0)(_*_)

  def sum2(list:List[Int]):Int = foldRight(list,0)(_+_)

  def product2(list:List[Double]):Double = foldRight(list,1.0)(_ * _)

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
