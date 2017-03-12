object week5 {
def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(y) => y
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(y) => List()
  case y :: ys => List(y) ++ init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case z :: zs => reverse(zs) ++ List(z)
}

def removeAt[T](xs: List[T], n: Int): List[T] = n match {
  case 0 => xs.tail
  case z => List(xs.head) ++ removeAt(xs.tail, n-1)
}

val h = List(1, 2, 3, 4)
val g = List(6, 7, 8, 9)

last(h)
init(h)

concat(h, g)

reverse(g)
reverse(h)

removeAt(h, 3)
removeAt(h, 0)

val fruit = List("apples", "oranges", "pears")
val diag3 = List(List(1,0,0), List(0,1,0), List(0,0,1))
val empty = List()

removeAt(fruit,1)
  removeAt(diag3,2)

}