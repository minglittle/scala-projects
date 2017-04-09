package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
        Math.pow(b(), 2) - 4 * a() * c()
      }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val b_ = Signal(-1 * b())
    val a2 = Signal(2 *a())
    val sd = Signal(Math.sqrt(delta()))

    Signal {
      if (delta() < 0) Set()
      else {
        Set(
          (b_() + sd()) / a2(),
          (b_() - sd()) / a2()
        )
      }
    }
  }
}
