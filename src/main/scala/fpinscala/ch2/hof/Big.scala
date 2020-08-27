package fpinscala.ch2.hof

object Big {
  def apply(i: BigInt): BigInt = i

  def unapply(n:BigInt) = Some(n.toInt)
}

