object test_5_3 {
  implicit class MyList[T](list: List[T]) {
    def myMap[U](f: T => U): List[U] = list match {
      case Nil => Nil
      case w :: ws => f(w) :: ws.myMap(f)
    }
  }
  val m = List(2.3, 3.5, 7.0, 2.5)

  val l = m.myMap(x => x * 2.0)
}

