package ru.bdm.neurons.gen

import ru.bdm.neurons.{Layer, NeuronSystem}

import scala.util.Random

object TestXor extends App {
  val quest = Seq(Seq(1.0, 1.0) -> Seq(.0), Seq(.0, .0) -> Seq(.0), Seq(0d, 1d) -> Seq(1d), Seq(1d, 0d) -> Seq(1d))

  val layer = Layer(2) * Layer(10) * Layer(10) * Layer(1)
  def appr(ns:NeuronSystem): Double = {
    1 to 10 map { _ =>
      val rn = Random.nextInt(quest.length)
      Math.pow(ns.work(quest(rn)._1).head - quest(rn)._2.head, 2)
    } sum
  }
  val alg = new GenAlgForNeuron(layer, appr)

  for(i <- 1 to 100) {
    alg.learnOne()
    print(i + " ")
    print(alg.bestResult + " ")
    printlnResult()
  }

  def printlnResult(): Unit = {
    quest.foreach(e => print(alg.chroms.head.work(e._1).head + " (" + e._2.head + ")"))
    println()
  }
}
