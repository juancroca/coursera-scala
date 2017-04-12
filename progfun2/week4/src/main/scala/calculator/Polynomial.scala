package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    (a(), b(), c()) match {
      case(ai, bi, ci) => Signal(bi * bi - (4 * ai * ci))
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    (a()*2, b(), delta()) match {
      case(ai, bi, di) => Signal({
        Math.sqrt(di) match {
          case dsqrt if dsqrt.isNaN => Set()
          case dsqrt if dsqrt == 0 => Set(-bi / ai)
          case dsqrt => Set((-bi - dsqrt)/ai, (-bi + dsqrt)/ai)
        }
      })
    }
  }
}
