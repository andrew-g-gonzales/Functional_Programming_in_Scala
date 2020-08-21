package fpinscala.ch2.hof.parially_applied

object Partially_Applied {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

}
