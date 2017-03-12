object lecture_6_1 {
  val xs = Array(1, 2, 3, 4)
  xs map (x => x * 2)

  val s = "Hello World !"
  s filter (c => c.isUpper)

  s exists (c => c.isUpper)
  s forall (c => c.isUpper)
  List("a", "b", "c") zip List(1, 2, 3)
  List((1, 2), (3, 4), (5, 6)).unzip
  s flatMap(c => List(".",c))
  xs.sum
  xs.product
  xs.max
  xs.min

  val M = 5
  val N = 5

  (1 to M) flatMap(x => (1 to N) map(y => (x,y)))

  def myProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1*xy._2).sum

  myProduct(Vector(1.0, 2.0, 3.0), Vector(3.0, 2.0, 1.0))

  def my2Product(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{case (x, y) => x * y}.sum

  my2Product(Vector(1.0, 2.0, 3.0), Vector(3.0, 2.0, 1.0))

  def myPrime(n: Int): Boolean =
    (2 until n).forall(d => (n % d) != 0)

  myPrime(5)
  myPrime(7)
  myPrime(10)

  val n = 11
  val l = (1 until n) map (i => (1 until i) map(j => (i,j)))

  l.flatten

  (1 until n) flatMap (i => (1 until i) map(j => (i,j))) filter (pair => myPrime(pair._1 + pair._2))

  val k = 5
  for {
    i <- 1 until k
    j <- 1 until i
    if myPrime(i + j)
  } yield (i, j)

  def myVProduct(xs: List[Double], ys: List[Double]): Double =
    {for {(i, j) <- xs zip ys} yield i*j}.sum

  myVProduct(List(1.0, 2.0, 3.0), List(2,0, 1.0, 3.0))

}