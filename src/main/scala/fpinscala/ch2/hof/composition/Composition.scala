package fpinscala.ch2.hof.composition

object Composition {

  def compose[A,B,C](g:A=>B,f:B=>C):A=>C = a => f(g(a))

}
