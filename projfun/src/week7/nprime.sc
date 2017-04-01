object week7 {
  def isPrime(n: Int): Boolean = {
    (2 until n) forall (n % _ != 0)
  }

  def secondPrime(from: Int, to: Int) =
    nthPrime(from, to, 2)

  def nthPrime(from: Int, to: Int, n: Int): Int =
    if (from >= to) throw new Error("no prime")
    else if (isPrime(from))
      if (n == 1) from else nthPrime(from + 1, to, n - 1)
    else nthPrime(from + 1, to, n)

  nthPrime(2, 11, 3)

  val xs = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

  (1 to 1000).toStream
  ((1000 to 10000).toStream filter isPrime) (0)

  object Obj2 {
    abstract class List[+T] {
      def isEmpty: Boolean
      def head: T
      def tail: List[T]
    }
    final case class ::[T](head: T, tail: List[T])
      extends List[T] {
      override def isEmpty: Boolean = false
    }
  }

//  class Stream[+T] {
//    if (isEmpty) this
//    else if (p(head)) cons(head, tail.filter(p))
//    else tail.filter(p)
//  }
//  def apply(n: Int): T =
//    if (n == 0) head
//    else tail.apply(n - 1)
  def from(n: Int): Stream[Int] = n #:: from(n+1)
  val nats = from(0)
  val m4s = nats map (_ * 4)
  (m4s take 100).toList
  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))
  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - x) / x) < 0.0001
  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }
  sqrtStream(2) take 10 toList
  //sqrtStream(4).take(10).toList
  sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList

}