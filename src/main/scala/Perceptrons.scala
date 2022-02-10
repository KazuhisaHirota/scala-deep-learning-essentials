package main.scala

class Perceptrons(val nInput: Int) {

  val nIn: Int = nInput
  val w: Array[Double] = new Array[Double](nInput)

  def train(x: Array[Double], t: Int, learningRate: Double): Int = {
    // check if the data is correctly classified
    var c: Double = 0.0
    for (i <- 0 until nIn) {
      c += w(i) * x(i) * t
    }

    var classified: Int = 0
    if (c > 0) { // the data is correctly classified
      classified = 1
    } else { // the data is wrongly classified
      // apply steepest descent method
      for (i <- 0 until nIn) {
        w(i) += learningRate * x(i) * t
      }
      println("w(0):" + w(0).toString + ", w(1):" + w(1).toString)
    }

    classified
  }

  def predict(x: Array[Double]): Double = {
    var preActivation: Double = 0.0
    for (i <- 0 until nIn) {
      preActivation += w(i) * x(i)
    }

    val sigma: ActivationFunction = new ActivationFunction()
    sigma.step(preActivation)
  }
}
