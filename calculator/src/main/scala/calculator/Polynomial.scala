package calculator

import scala.math.sqrt

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(b.apply() * b.apply() - 4 * a.apply() * c.apply())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(if (delta.apply() < 0) Set() else Set((-b.apply() + sqrt(delta.apply())) / (2 * a.apply()), (-b.apply() - sqrt(delta.apply())) / (2 * a.apply())))
  }
}
