object myMap_5 {
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( x => f(x) )

  def lengthFun[T](xs: list[T]): Int =
    (xs foldRight 0)( x => x + 1 )

  val m = List(1, 2, 3)
  val n = mapFun(m, x => x + 1)l
}