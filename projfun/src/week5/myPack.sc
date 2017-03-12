object myPack {
  def myPack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case z :: zs => List(xs.takeWhile(x => x == z),
  }
}