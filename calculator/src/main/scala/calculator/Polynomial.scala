package calculator

import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(delta() match {
      case d if d < 0 => Set()
      case d if d == 0 => Set(-b() / 2 * a())
      case d =>
        val aVal = a()
        val bVal = -b()
        val sqrtVal = sqrt(d)
        Set((bVal + sqrtVal) / 2 * aVal, (bVal - sqrtVal) / 2 * aVal)
    })
  }
}
