package ru.bdm.neurons

import scala.util.Random

class Neuron(val id: Int, val inputs: Array[Int], val activate: Double => Double = Func(Func.sigmoid), val neurons: Int => Neuron) {

  val weights: Array[Double] = new Array[Double](inputs.length + 1)

  private var result_p: Double = 0
  private var is_be: Boolean = false

  def result(): Double = if (!is_be) {
    is_be = true
    work(inputs.map(neurons(_).result()))
  } else result_p

  def work(in: Seq[Double]): Double = {
    val q = in.zip(weights).foldLeft(.0) { case (sum, (input, weight)) ⇒ sum + input * weight }
    is_be = true
    result_p = activate(q + weights.last)
    result_p
  }

  def setWeights(ws: Seq[Double]): Unit = {
    for (i ← weights.indices)
      weights(i) = ws(i)
  }

  def setRandomWeight(): Neuron = {
    for (i ← weights.indices)
      weights(i) = Random.nextDouble()
    weights(weights.length - 1) = Random.nextDouble() * 2 - 1.0
    this
  }

  def update(): Unit =
    is_be = false
}
