package fpinscala.ch2.hof.polymorphic

object Find {

  def findFirst[A](a:Array[A], p:A=>Boolean):Int ={

    @annotation.tailrec
    def loop(n:Int): Int ={
      if(n >= a.length){
        -1
      }else if(p(a(n))){
        n
      }else{
        loop(n+1)
      }
    }

    loop(0)
  }

  def findFirstMonomorphic(a :Array[String], key:String): Int ={

    @annotation.tailrec
    def loop(n:Int): Int ={
      if(n>= a.length){
        -1
      }else if(a(n) == key){
        n
      }else{
        loop(n+1)
      }
    }

    loop(0)
  }



}
