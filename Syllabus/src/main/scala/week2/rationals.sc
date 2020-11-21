class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  // define another using the predefined constructor
  def this(x: Int) = this(x, 1)

  @scala.annotation.tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)


  def numer = x
  def denom = y

  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational): Rational =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom
    )

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational): Rational = add(that.neg)


  override def toString = {
    val g = gcd(x, y)
    numer / g + "/" + denom / g
  }
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

y.add(y)
x.sub(y).sub(z)
