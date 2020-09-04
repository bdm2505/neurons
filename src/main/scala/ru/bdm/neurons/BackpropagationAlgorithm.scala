package ru.bdm.neurons

import scala.collection.mutable


class BackpropagationAlgorithm(val ns: NeuronSystem) {

  val isBe: Array[Boolean] = new Array(ns.neurons.length)
  val errors: Array[Double] = new Array(ns.neurons.length)
  var queue: mutable.Queue[Int] = mutable.Queue.empty
  var sumError:Double = 0

  def derivative(d: Double): Double = d * (1 - d)

  def teach(inputs:Seq[Double], rights: Seq[Double], speed:Double = 0.001): Unit = {
    ns.work(inputs)
    for (i <- isBe.indices) {
      isBe(i) = false
    }
    for (i <- errors.indices)
      errors(i) = 0
    sumError = 0

    ns.outputs.zip(rights).foreach { case (neuron, right) =>
      sumError += Math.abs(right - neuron.work())
      errors(neuron.id) = derivative(neuron.work()) * (right - neuron.work())
      queue enqueue neuron.id
    }

    while (queue.nonEmpty) {
      ns.neurons(queue.dequeue()) match {
        case neuron: NeuronOut if !isBe(neuron.id) =>
          isBe(neuron.id) = true
          neuron.inputs.zip(neuron.weights).map { case (id, weight) =>
            errors(id) = (errors(id) + weight * errors(neuron.id)) / 2.0
            if(!isBe(id))
              queue enqueue(id)
            for (i <- 0 until (neuron.weights.length - 1))
              neuron.weights(i) += errors(neuron.id) * speed * ns.neurons(neuron.inputs(i)).work()
            neuron.weights(neuron.weights.length - 1) += errors(neuron.id) * speed
          }
        case _ =>
      }
    }
  }

}
