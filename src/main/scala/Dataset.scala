package main.scala

import scala.util.Random

final class Dataset {

  def makeDataset(start: Int, end: Int, mu1: Double, mu2: Double, answer: Int,
                  x: Array[Array[Double]], t: Array[Int]): Unit = {
    for (i <- start until end) {
      x(i)(0) = Random.nextGaussian() + mu1 // input variable 1
      x(i)(1) = Random.nextGaussian() + mu2 // input variable 2
      t(i) = answer
    }
  }
}
