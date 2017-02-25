package week4

class PatternMatching extends FunSuite with ShouldMatchers {

  trait Expr {
    def eval: Int = this match {
      case class Number (n: Int) extends Expr
      case class Sum (e1: Expr, e2: Expr) extends Expr
      case class Prod (e1: Expr, e2: Expr) extends Expr
    }
  }
}


object exprs {
  def show(e: Expr) = e match {
    case Number(x) => x.toString
    case Var(x) => x
    case Sum(l, r) => show(l) + " + " + show(r)
    case Prod(l, r) => {
      def showBracketsForSums(e: Expr): String = e match {
        case Sum(x, y) => " ( " + e.show + " ) "
        case _ => e.show
      }
      showBracketsForSums(l) + " * " + showBracketsForSums(r)
    }
  }

  show(Sum(Number(1), Number(2)))
  show(Sum(Prod(2, Var("x")), Var("y")))
  show(Prod(Sum(2, Var("x")), Var("y")))
}