class Rational(n: Int, d: Int) {

  require(d != 0)
  private val g = gcd(n.abs, d.abs)
  val numer = n / g
  val denom = d / g

  def this(n: Int) = this(n, 1)

  def +(that: Rational): Rational =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom
    )
  // Leaving out for demo implicit
  // def +(i: Int): Rational

  def *(that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def *(i: Int): Rational =
    new Rational(numer * i, denom)

  override def toString = numer + "/" + denom

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

}

object Rational {

  def main(argv: Array[String]) {
    val x = new Rational(8, 14)
    println(x)
    println(x * 3)

    /** Global implicit conversion rule!
     */
    implicit def intToRational(x: Int) = new Rational(x)

    println(x + 1)
  }
}
