package ru.bdm.neurons

import java.io.{File, PrintWriter}

import org.json4s.NoTypeHints
import org.json4s.native.Serialization

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

class NeuronSystem(numberNeurons:Int, val model: Seq[NeuronModel]) {
  val neurons:Array[Neuron] = new Array(numberNeurons)

  var inputs:Seq[Neuron] = Seq.empty
  var outputs:Seq[Neuron] = Seq.empty

  def addNeurons(ns:Seq[Neuron]): Unit = {
    ns.foreach{ neuron =>
      if (neurons(neuron.id) != null)
        throw new Exception(s"id=${neuron.id} neuron already exist")
      neurons(neuron.id) = neuron
    }
  }
  def work(in:Seq[Double]):Seq[Double] = {
    neurons.foreach(_.update())
    inputs foreach (_.work(in))
    outputs.map(_.result())
  }

  def write():NeuronSystemWrite = {
    NeuronSystemWrite(model, neurons.foldRight[Seq[WeightsWrite]](Seq.empty){ case (neuron, arr) =>
      neuron match {
        case out: Neuron => arr :+ WeightsWrite(out.id, out.weights)
        case _ => arr
      }
    })
  }

  def saveToFile(fileName:String): Unit = {
    implicit val formats = Serialization.formats(NoTypeHints)
    val wr = new PrintWriter(new File(fileName))
    wr.write(Serialization.write(write()))
    wr.close()
  }
}

object NeuronSystem {
  implicit val formats = Serialization.formats(NoTypeHints)

  def create(model:Seq[NeuronModel]): NeuronSystem = {
    val neuronSystem = new NeuronSystem(model.length, model)
    val inputs = model.filter(_.tag == NeuronTag.input).map(_.create(neuronSystem.neurons))
    val outputs = model.filter(_.tag == NeuronTag.output).map(_.create(neuronSystem.neurons))
    val rest = model.filterNot(md => md.tag == NeuronTag.input || md.tag == NeuronTag.output).map(_.create(neuronSystem.neurons))


    neuronSystem.addNeurons(inputs)
    neuronSystem.addNeurons(rest)
    neuronSystem.addNeurons(outputs)
    neuronSystem.neurons.foreach(_.setRandomWeight())
    neuronSystem.inputs = inputs.toIndexedSeq
    neuronSystem.outputs = outputs.toIndexedSeq
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

  def readFromFile(nameFile:String)  = {
    val source = Source.fromFile(nameFile)
    var str = source.getLines().mkString(" ")
    val ns = Serialization.read[NeuronSystemWrite](str).create()
    source.close()
    ns
  }
}
