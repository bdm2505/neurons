package ru.bdm.neurons

import scala.util.Random

class NeuronOut(val id:Int, val inputs:Array[Int], val activate: Double => Double = Func(Func.sigmoid), val neurons: Int => Neuron) extends Neuron {

  override val weights: Array[Double] = new Array[Double](inputs.length + 1)

  var result: Option[Double] = None

  def work(): Double = result.getOrElse {
    val q = inputs.zip(weights).foldLeft(.0) { case (sum, (id, weight)) ⇒ sum + neurons(id).work() * weight }
    result = Some(activate(q + weights.last))
    result.get
  }
  def setWeights(ws: Seq[Double]): Unit = {
    for(i ← weights.indices)
      weights(i) = ws(i)
  }
  def setRandomWeight(): Neuron = {
    for(i ← weights.indices)
      weights(i) = Random.nextDouble()
    weights(weights.length - 1) = Random.nextDouble() * 2 - 1.0
    this
  }

  override def update(): Unit =
    result = None
}
