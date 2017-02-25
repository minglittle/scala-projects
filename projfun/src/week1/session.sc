import scala.annotation.tailrec

object session {
  1 + 2

  15 % 3

  def gcd(a: Int, b: Int): Int =
    if (b==0) a else gcd(b, a % b)

  gcd(14, 21)
  gcd(60, 24)

  @tailrec
  def factorial(n: Int): Int =
    if (n==0) 1 else n * factorial(n - 1)

  factorial(3)
  factorial(5)

  println("Hello, Welcome to Scala programming at Adtran")

  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      abs(x - guess * guess) / abs(x) < 0.0001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(0.001)
  sqrt(0.1e-20)
  sqrt(1.0e20)
  sqrt(1.0e50)

  val x = 0
  def f(y: Int) = y + 1
  val result = {
    val x = f(3)
    x * x
  }

}
