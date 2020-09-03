package ru.bdm.neurons

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write, writePretty}


object Main {
  def main(args: Array[String]): Unit = {
    implicit val formats = Serialization.formats(NoTypeHints)
    val in = Seq(0.5, 0.2)
    val neurons = NeuronSystem.createStandardModel(Seq(2, 2, 1))
    println(neurons)
    println(neurons.work(in))

    println(writePretty(neurons.write))
    val a = write(neurons.write)

    val newNeurons = read[NeuronSystemWrite](a).create
    println(newNeurons)
    println(newNeurons.work(in))
  }
}
