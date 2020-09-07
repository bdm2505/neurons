package ru.bdm.neurons

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class BackpropagationAlgorithm(val ns: NeuronSystem, var speed:Double = 0.5) {

  val isBe: Array[Boolean] = new Array(ns.neurons.length)
  val errors: Array[Double] = new Array(ns.neurons.length)
  var list: List[Int] = Nil
  var sumError: Double = 0


  def derivative(d: Double): Double = d * (1 - d)

  def teach(inputs: Seq[Double], rights: Seq[Double], parallel:Boolean = false): Unit = {
    ns.work(inputs)
    for (i <- isBe.indices) {
      isBe(i) = false
    }
    for (i <- errors.indices)
      errors(i) = 0
    sumError = 0

    ns.outputs.zip(rights).foreach { case (neuron, right) =>
      sumError += Math.abs(right - neuron.result())
      errors(neuron.id) = (right - neuron.result())
      list ::= neuron.id
    }

    while (list.nonEmpty) {
      val nq = list
      list = Nil
      nq foreach { id =>
        if (parallel) Future {
          calculate(id)
        } else
          calculate(id)
      }
    }
  }

  private def calculate(id: Int) = {
    ns.neurons(id) match {
      case neuron: Neuron if !isBe(neuron.id) =>
        isBe(neuron.id) = true
        errors(neuron.id) *= derivative(neuron.result())
        neuron.inputs.zip(neuron.weights).foreach { case (id, weight) =>
          errors(id) += weight * errors(neuron.id)
          if (!isBe(id))
            list ::= id
        }
        for (i <- 0 until (neuron.weights.length - 1))
          neuron.weights(i) += errors(neuron.id) * speed * ns.neurons(neuron.inputs(i)).result()
        neuron.weights(neuron.weights.length - 1) += errors(neuron.id) * speed
      case _ =>
    }
  }
}
