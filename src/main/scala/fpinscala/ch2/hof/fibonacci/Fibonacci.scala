package fpinscala.ch2.hof.fibonacci

object Fibonacci {

  def fibonacci_1(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => {
      val n1 = fibonacci_1(n - 1)
      val n2 = fibonacci_1(n - 2)
      val f =n1 + n2
      f
    }
  }

}
