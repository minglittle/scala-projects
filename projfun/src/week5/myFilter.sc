object test_5_4 {
  abstract class List[T] {
    def myFilter(p: T => Boolean): List[T] = this match {
      case Nil => this
      case z :: zs => if (p(z)) z :: zs.myFilter(p) else zs.myFilter(p)
    }
  }
  def posElems(xs: List[Int]): List[Int] = xs myFilter (x => x > 0)

  val m = List(1, 2, 3, -1, -2)
  val n = posElems(m)
}