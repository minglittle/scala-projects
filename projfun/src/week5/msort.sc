object week5_m {
  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length/2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
        case Nil => ys
        case z :: zs => ys match {
          case Nil => xs
          case w :: ws =>
            if (z < w) z :: merge(zs, ys)
            else w :: merge(xs, ws)
        }
      }
      def merge2(xs: List[Int], ys: List[Int]):List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (w :: ws, z :: zs) =>
          if (w < z) w :: merge2(ws, ys)
          else z :: merge2(xs, zs)
      }

      val (fst, snd) = xs splitAt n
      merge2(msort(fst), msort(snd))
    }
  }

  def msort2[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length/2
    if (n == 0) xs
    else {
      def merge3(xs: List[T], ys: List[T]):List[T] =
        (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (w :: ws, z :: zs) =>
          if (lt(w, z)) w :: merge3(ws, ys)
          else z :: merge3(xs, zs)
      }

      val (fst, snd) = xs splitAt n
      merge3(msort2(fst)(lt), msort2(snd)(lt))
    }
  }
  val m = List(5, 9, 1, 4, 7, 0)

  val l = msort2(m)((a:Int, b:Int) => a < b)

  val pair: (String, Int) = ("Answer", 5)
  val (label: String, value: Int) = pair

  val fruit = List("pineapple", "apple", "orange")
  val fruit_s = msort2(fruit)((a:String, b:String) => a.compareTo(b) < 0)

}