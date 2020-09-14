package ru.bdm.neurons

import scala.util.Random

object Main1 extends App {

  val ns = NeuronSystem.createStandardModel(Seq(2,10,20,10,20,10,1))
  val bpa = new BackpropagationAlgorithm(ns)

  Main.printTime("te") {
    for(i <- 1 to 1000) {
      val input = Seq(0.2, 0.5)
      val out = Seq(0.9)

      bpa.teach(input, out)
      println(i + ": " + (1 - bpa.sumError))
    }
  }
}
