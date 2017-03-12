object lecture_6_4 {
  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Switherland" -> "Bern")

  romanNumerals("V")
  capitalOfCountry("US")

  romanNumerals get "I"
  capitalOfCountry get "Andorra"

  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "missing data"
  }

  showCapital("US")
  showCapital("Andorra")

  val fruit = List("apple", "pear", "orange", "pineapple")
  fruit sortWith (_.length < _.length)
  fruit.sorted
  fruit groupBy (_.head)

  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = terms0 withDefaultValue (0.0)

    def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
      // terms get exp match {
      //  case Some(coeff1) => exp -> (coeff + coeff1)
      //  case None => exp -> coeff
    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }
  //val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  //val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
  p1 + p2
  p1.terms(7)

  val cap1 = capitalOfCountry withDefaultValue("<unknown>")
  cap1("Andorra")
}