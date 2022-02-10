package main.scala

final class ActivationFunction {

  def step(x: Double): Int = {
    if (x >= 0.0) {
      1
    } else {
      -1
    }
  }

  def sigmoid(x: Double): Double = {
    1.0 / (1.0 + math.exp(-x))
  }

  def softmax(x: Array[Double]): Array[Double] = {
    val n: Int = x.length

    var max: Double = 0.0
    for (i <- 0 until n) {
      if (x(i) > max) max = x(i)
    }

    val e: Array[Double] = new Array[Double](n)
    var sum: Double = 0.0
    for (i <- 0 until n) {
      e(i) = math.exp(x(i) - max) // to avoid overflow
      sum += e(i)
    }
    for (i <- 0 until n) e(i) /= sum
    e
  }
}
