package ru.bdm.neurons

import scala.collection.mutable
import scala.util.Random

class NeuronSystem(numberNeurons:Int, val model: Seq[NeuronModel]) {
  val neurons:Array[Neuron] = new Array(numberNeurons)

  var inputs:Seq[NeuronInput] = Seq.empty
  var outputs:Seq[NeuronOut] = Seq.empty

  def addNeurons(ns:Seq[Neuron]): Unit = {
    ns.foreach(neuron => neurons(neuron.id) = neuron)
  }
  def work(in:Seq[Double]):Seq[Double] = {
    inputs zip in foreach { case (neuron, in) => neuron.value = in }
    neurons.foreach(_.update())
    outputs.map(_.work())
  }

  def write():NeuronSystemWrite = {
    NeuronSystemWrite(model, neurons.foldRight[Seq[WeightsWrite]](Seq.empty){ case (neuron, arr) =>
      neuron match {
        case out: NeuronOut => arr :+ WeightsWrite(out.id, out.weights)
        case _ => arr
      }
    })
  }
}

object NeuronSystem {

  def create(model:Seq[NeuronModel]): NeuronSystem = {
    val neuronSystem = new NeuronSystem(model.length, model)
    val inputs = model.filter(_.tag == NeuronTag.input).map(model => new NeuronInput(model.id))
    val outputs = model.filter(_.tag == NeuronTag.output).map(model => new NeuronOut(model.id, model.inputs, Func(model.func), neuronSystem.neurons.apply))
    val rest = model.filterNot(md => md.tag == NeuronTag.input || md.tag == NeuronTag.output).map(model => new NeuronOut(model.id, model.inputs, Func(model.func), neuronSystem.neurons.apply))
    rest.foreach(_.setRandomWeight())
    outputs.foreach(_.setRandomWeight())
    neuronSystem.addNeurons(inputs)
    neuronSystem.addNeurons(rest)
    neuronSystem.addNeurons(outputs)
    neuronSystem.inputs = inputs
    neuronSystem.outputs = outputs
    neuronSystem
  }

  def createStandardModel(model:Seq[Int]): NeuronSystem = {
    val input = 0 until model.head map(id => NeuronModel(id, tag = NeuronTag.input))
    var count = model.head
    var ids = input.map(_.id)
    var rest:Seq[NeuronModel] = Seq.empty
    for(num <- model.slice(1, model.length - 1)) {
      val layer =  count until (count + num) map (id => NeuronModel(id, ids))
      count += num
      ids = layer.map(_.id)
      rest ++= layer
    }
    val output = count until (count + model.last) map(id => NeuronModel(id, ids, NeuronTag.output))

    create(input ++ rest ++ output)
  }
}
