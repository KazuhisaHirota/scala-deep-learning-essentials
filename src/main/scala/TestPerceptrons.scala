package main.scala

import scala.util.Random
import scala.util.control.Breaks

object TestPerceptrons {

  def run(): Unit = {
    println("set configs")

    Random.setSeed(1234)

    val trainN: Int = 50 // number of training data
    val trainSize: Int = trainN / 2 // TODO rename

    val testN: Int = 10 // number of input data
    val testSize: Int = testN / 2 // TODO rename

    val nIn: Int = 2 // dimension of input data
    // nOut: Int = 1

    val epochs: Int = 100
    val learningRate: Double = 1.0 // learning rate can be 1 in perceptrons

    println("initialize tensors")

    // training data
    val trainX: Array[Array[Double]] = Array.ofDim[Double](trainN, nIn) // input data for training
    val trainT: Array[Int] = new Array[Int](trainN) // answers (labels) for training

    // test data
    val testX: Array[Array[Double]] = Array.ofDim[Double](testN, nIn) // input data for test
    val testT: Array[Int] = new Array[Int](testN) // answers (labels) for test

    println("make dataset")

    val maker: Dataset = new Dataset

    // class1 inputs x11 and x12: x11 ~ N(-2.0, 1.0), x12 ~ N(+2.0, 1.0)
    val mu11: Double = -2.0
    val mu12: Double = 2.0
    val answer1: Int = 1
    // make training data
    maker.makeDataset(0, trainSize, mu11, mu12, answer1, trainX, trainT)
    // make test data
    maker.makeDataset(0, testSize, mu11, mu12, answer1, testX, testT)

    // class2 inputs x21 and x22: x21 ~ N(+2.0, 1.0), x22 ~ N(-2.0, 1.0)
    val mu21: Double = 2.0
    val mu22: Double = -2.0
    val answer2: Int = -1
    // make training data
    maker.makeDataset(trainSize, trainN, mu21, mu22, answer2, trainX, trainT)
    // make test data
    maker.makeDataset(testSize, testN, mu21, mu22, answer2, testX, testT)

    // build model

    // construct
    val classifier: Perceptrons = new Perceptrons(nIn)

    // train
    println("train data")
    var epoch: Int = 0 // training epoch counter
    val loop = new Breaks
    loop.breakable {
      while (true) {
        println("epoch: " + epoch.toString)

        var classified: Int = 0
        for (i <- 0 until trainN) {
          classified += classifier.train(trainX(i), trainT(i), learningRate)
        }
        if (classified == trainN) { // all data are classified correctly
          loop.break()
        }

        epoch += 1
        if (epoch > epochs) {
          loop.break()
        }
      }
    }

    // test
    println("test")
    val predictedT: Array[Double] = new Array[Double](testN)
    // prediction by trained model
    for (i <- 0 until testN) {
      predictedT(i) = classifier.predict(testX(i))
    }

    // evaluate the model
    val confusionMatrix: Array[Array[Double]] = Array.ofDim[Double](2, 2)
    var accuracy: Double = 0.0
    var precision: Double = 0.0
    var recall: Double = 0.0

    for (i <- 0 until testN) {
      if (predictedT(i) > 0) { // positive
        if (testT(i) > 0) { // TP
          accuracy += 1
          precision += 1
          recall += 1
          confusionMatrix(0)(0) += 1
        } else { // FP
          confusionMatrix(1)(0) += 1
        }
      } else { // negative
        if (testT(i) > 0) { // FN
          confusionMatrix(0)(1) += 1
        } else { // TN
          accuracy += 1
          confusionMatrix(1)(1) += 1
        }
      }
    }

    accuracy /= testN
    precision /= confusionMatrix(0)(0) + confusionMatrix(1)(0) // TP / (TP + FP)
    recall /= confusionMatrix(0)(0) + confusionMatrix(1)(0) // TP / (TP + FN)

    println("Perceptrons model evaluation")
    println("Accuracy: " + (accuracy * 100).toString)
    println("Precison: " + (precision * 100).toString)
    println("Recall: " + (recall * 100).toString)
  }

  def main(args: Array[String]): Unit = {
    run()
  }
}
