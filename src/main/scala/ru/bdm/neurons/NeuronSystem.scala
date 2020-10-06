package ru.bdm.neurons

import java.io.{File, PrintWriter}

import org.json4s.NoTypeHints
import org.json4s.native.Serialization

import scala.io.Source
import scala.util.Random

class NeuronSystem(val model: Layer) {


  val neurons: Array[Neuron] = new Array(model.size)

  model.neurons.foreach{ case (id, neuronModel) =>
    neurons(id) = neuronModel.create(id, neurons)
  }


  val inputs: Array[Neuron] = model.inputs.map(neurons(_)).toArray
  val outputs: Array[Neuron] = model.outputs.map(neurons(_)).toArray


  def work(in: Seq[Double]): Seq[Double] = {
    neurons.foreach(_.update())
    inputs.zip(in).foreach{ case (n, input) => n.work(Seq(input))}
    outputs.map(_.result())
  }

  def write(): NeuronSystemWrite = {
    NeuronSystemWrite(model, neurons.map{ n=> WeightsWrite(n.id, n.weights)})
  }

  def setRandomWeights(rand: Random = Random): NeuronSystem = {
    neurons.foreach(_.setRandomWeight(rand))
    this
  }

  def saveToFile(fileName: String): Unit = {
    implicit val formats = Serialization.formats(NoTypeHints)
    val wr = new PrintWriter(new File(fileName))
    wr.write(Serialization.write(write()))
    wr.close()
  }
  def setWeights(newWeights: Seq[Seq[Double]]): Unit = {
    neurons.zip(newWeights).foreach { case (n, w) =>
      n.setWeights(w)
    }
  }

  override def toString: String = s"model=$model weight=${neurons.mkString("{", ", ", "}")}"
}

object NeuronSystem {
  implicit val formats = Serialization.formats(NoTypeHints)

  def apply(layer: Layer): NeuronSystem = {
    new NeuronSystem(layer).setRandomWeights()
  }

  def createStandardModel(model: Seq[Int]): NeuronSystem = {
    apply(Layer.model(model))
  }

  def readFromFile(nameFile: String) = {
    val source = Source.fromFile(nameFile)
    var str = source.getLines().mkString(" ")
    val ns = Serialization.read[NeuronSystemWrite](str).create()
    source.close()
    ns
  }
}
