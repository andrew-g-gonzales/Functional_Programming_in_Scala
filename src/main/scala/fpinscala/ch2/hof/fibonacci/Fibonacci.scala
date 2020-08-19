package fpinscala.ch2.hof.fibonacci

import fpinscala.ch2.hof.Big

object Fibonacci {

  def fibonacci_2(n:BigInt):BigInt = {

    def loop(n:BigInt, prev:BigInt, curr:BigInt):BigInt = {
      if(n <= 1){
        curr
      }else{
        loop(n-1,curr,prev+curr)
      }
    }
    loop(n,0,1)
  }

  def fibonacci_1(n: BigInt): BigInt = n match {
    case Big(0) | Big(1) => n
    case _ => {
      val n1 = fibonacci_1(n - 1)
      val n2 = fibonacci_1(n - 2)
      val f =n1 + n2
      f
    }
  }

}
