package ru.bdm.neurons

import scala.util.Random

class Neuron(
              val id: Int,
              val neurons: Int => Neuron,
              val inputs: Array[Int],
              val activate: Double => Double = Func(Func.sigmoid),
              val derivative: Double => Double = Func.derivative(Func.sigmoid)) {

  val weights: Array[Double] = new Array[Double](Math.max(inputs.length, 1))

  private var result_p: Double = 0
  private var visited: Boolean = false

  def result(): Double = if (!visited) {
    work(inputs.map(neurons(_).result()))
  } else result_p

  def work(in: Seq[Double]): Double = {
    val sum = in.zip(weights).foldLeft(.0) {
      case (sum, (input, weight)) ⇒ sum + input * weight
    } + (if (in.isEmpty) weights.head else 0)
    visited = true
    result_p = activate(sum)
    //println(s"work:  inputs=$in sum=$sum result=$result_p")
    result_p
  }

  def setWeights(ws: Seq[Double]): Unit = {
    for (i ← weights.indices)
      weights(i) = ws(i)
  }

  def setRandomWeight(rand: Random): Neuron = {
    for (i ← weights.indices)
      weights(i) = rand.nextDouble() * 20 - 10.0
    this
  }

  def update(): Unit =
    visited = false

  override def toString: String = s"Neuron(${weights.mkString(", ")})"
}
