object test_5_3 {
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case z :: zs => z*z :: squareList(zs)
  }
  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case z :: zs => if (z > 0) z :: posElems(zs) else posElems(zs)
  }

  val m = List(2, -5, 3, 7, 2, -2)
  val x = squareList(m)

  val n = posElems(m)
}