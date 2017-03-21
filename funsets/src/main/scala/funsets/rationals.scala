package funsets

object rationals{

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  val t1 = x + y + z

  val t2 = -x

  val t3 = x - y - z

  val t4 = x < y

  val t5 = x max y
}

class Rational (x: Int, y: Int) {

  private def gcd(a: Int, b: Int):Int = if (b == 0) a else gcd(b,a%b)

  val numer = x
  val denom = y

  def unary_- : Rational = new Rational(-numer,denom)

  def +(that : Rational) = new Rational(
    numer * that.denom + that.numer * denom, denom * that.denom
  )

  def - (that : Rational) = this + -that

  def <(that : Rational) = numer * that.denom < denom * that.numer

  def max(that : Rational) = if(this < that) that else this

  override def toString = {
    val g = gcd(x,y)
    numer/g + "/" + denom/g
  }
}
