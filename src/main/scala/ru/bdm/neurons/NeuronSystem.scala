package ru.bdm.neurons

import scala.collection.mutable
import scala.util.Random

class NeuronSystem(val inputs:Seq[InputNeuron], val outs:Seq[Out]) {
  val neurons:mutable.Map[Int,Neuron] = mutable.Map()

  def work(in:Seq[Double]):Seq[Double] = {
    inputs zip in foreach{case (input , value) ⇒ input.value = value}
    outs.map(_.update())
  }
  def write: NeuronSystemWrite = {
    outs foreach findNeurons
    NeuronSystemWrite(inputs.map(_.write), neurons.view.map(_._2.write).toSeq)
  }
  def findNeurons(out: Out): Unit = {
    out match {
      case neuron: Neuron if !neurons.contains(neuron.id) ⇒
        neurons += neuron.id → neuron
        neuron.input foreach findNeurons
      case _ ⇒
    }
  }



}

object NeuronSystem {
  def createStandardModel(model:Seq[Int]) = {
    val inputs = 1 to model.head map (_ ⇒ InputNeuron(Random.nextDouble()))
    val outs = model.tail.foldLeft(inputs.asInstanceOf[Seq[Out]]){case (inputs, number) ⇒ 1 to number map (_ ⇒ Neuron(inputs).setRandomWeight())}
    new NeuronSystem(inputs, outs)
  }
}
