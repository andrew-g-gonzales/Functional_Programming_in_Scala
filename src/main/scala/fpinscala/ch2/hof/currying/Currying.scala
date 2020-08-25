package fpinscala.ch2.hof.currying

object Currying {

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  val pred:Int=>Boolean = (y:Int) => y<10
  val higher : (Int=>Boolean) => (Int=>Boolean) = (k:Int=>Boolean) => pred
}
