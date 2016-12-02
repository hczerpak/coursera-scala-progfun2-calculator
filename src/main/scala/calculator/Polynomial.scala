package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = Signal(b()*b() - 4*a()*c())

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double],
                       delta: Signal[Double]): Signal[Set[Double]] = {

    val delta = computeDelta(a, b, c)
    val dsqrt =  Math.sqrt(delta())
    if (delta() < 0) Signal(Set.empty)
    else if (delta() == 0) Signal(Set(-b()/2*a())) // -b / 2a
    else Signal(Set((-b() + dsqrt) / 2 * a(), (-b() - dsqrt) / 2 * a())) //(-b ± √Δ) / 2a
  }
}
