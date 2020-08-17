package fpinscala.ch2.hof.factorial

object Factorial {

  def factorial2(n:Int):BigInt ={
    if(n==0){
      1
    }else{
      n * factorial2(n-1)
    }
  }

  def factorial(n:Int): BigInt ={

    @annotation.tailrec
    def go(n:Int, acc:BigInt):BigInt ={
      if(n ==0){
        acc
      }else{
        go(n-1,acc*n)
      }
    }
      go(n,1)
  }

}
