package ru.bdm.neurons

import scala.util.Random

class Neuron(val input: Seq[Out], val id:Int = Neuron.nextId(), val weights: Array[Double]) extends Out {

  var result: Option[Double] = None

  override def out: Double = result.getOrElse(work())

  override def update():Double = {
    if (result.isDefined) {
      result = None
      input.foreach(_.update())
    }
    out
  }

  def activate(x:Double): Double = 1.0 / (1 + Math.pow(Math.E, -x) )

  def work(): Double = {
    val q = input.zip(weights).foldLeft(.0) { case (sum, (in, weight)) ⇒ sum + in.out * weight }
    result = Some(activate(q + weights.last))
    result.get
  }
  def setRandomWeight(): Neuron = {
    for(i ← weights.indices)
      weights(i) = Random.nextDouble()
    this
  }
  def write: NeuronWrite = NeuronWrite(id, input.map(_.id), weights)
}
object Neuron{
  private var ids = 0
  def nextId():Int = {
    ids+=1
    ids
  }
  def apply(input: Seq[Out], id:Int = nextId()): Neuron = new Neuron(input, id, new Array(input.size + 1))


}
