object rationals {
  //  println("Welcome to the Scala worksheet for Rational")

  val x = new Rational(1, 3)
  x.numer
  x.denom

  class Rational(x: Int, y: Int) {
    def numer = x
    def denom = y
    def neg: Rational = new Rational(-numer,denom)
    def add(that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )
    def sub(that: Rational) = add(that.neg)

    def mul(that: Rational) = new Rational(numer * that.numer, denom * that.denom)

    override def toString = numer + "/" + denom
  }

  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x.neg
  x.add(y)
  x.mul(y)
  x.sub(y)
  x.add(y).mul(z)
  makeString(x.add(y))

def addRational(r: Rational, s: Rational): Rational =
    new Rational((r.numer * s.denom + s.numer * r.denom), r.denom * s.denom)

  def makeString(r: Rational) = r.numer + "/" + r.denom

  makeString(addRational(new Rational(1, 2), new Rational(2, 3)))
}