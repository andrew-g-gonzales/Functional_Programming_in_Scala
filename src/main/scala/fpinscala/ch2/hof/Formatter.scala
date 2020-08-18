package fpinscala.ch2.hof

object Formatter {

  def formatResult(name: String, n: BigInt, f: BigInt => BigInt) = "The %s of %d is %d".format(name, n, f(n))

}
