package fpinscala.ch2.hof

object Big {
  def unapply(n:BigInt) = Some(n.toInt)
}

