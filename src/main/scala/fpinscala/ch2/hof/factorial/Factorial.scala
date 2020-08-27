package fpinscala.ch2.hof.factorial

import fpinscala.ch2.hof.Big

object Factorial {

def factorial2(n:BigInt): BigInt ={
  if(n ==0){
    1
  }else{
    n * factorial2(n-1)
  }
}

  def factorial(n:BigInt):BigInt = {

    @annotation.tailrec
    def go(n:BigInt,acc:BigInt): BigInt = {
      if(n ==0){
        acc
      }else{
        go(n-1,n*acc)
      }
    }

    go(n,1)
  }

}
