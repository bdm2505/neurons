package ru.bdm.neurons

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class BackpropagationAlgorithm(val ns: NeuronSystem, var speed: Double = 0.5) {

  val isBe: Array[Boolean] = new Array(ns.neurons.length)

  val errors: Array[Double] = new Array(ns.neurons.length)
  var set: Set[Int] = Set.empty
  var sumError: Double = 0


  def derivative(d: Double): Double = d * (1 - d)

  def teach(inputs: Seq[Double], rights: Seq[Double], parallel: Boolean = false): Unit = {
    ns.work(inputs)
    println("teach start")
    for (i <- isBe.indices) {
      isBe(i) = false
    }
    for (i <- errors.indices)
      errors(i) = 0
    sumError = 0

    ns.outputs.zip(rights).foreach { case (neuron, right) =>
      sumError += Math.abs(right - neuron.result())
      errors(neuron.id) = (right - neuron.result())
      set += neuron.id
    }

    while (set.nonEmpty) {
      val oldSet = set
      println(s"set=$oldSet")
      set = Set.empty
      oldSet foreach { id =>
        if (parallel) Future {
          calculate(id)
        } else
          calculate(id)
      }
    }
  }

  private def calculate(id: Int): Unit = {
    ns.neurons(id) match {
      case neuron: Neuron if !isBe(neuron.id) =>
        isBe(neuron.id) = true
        errors(neuron.id) *= derivative(neuron.result())

        (neuron match {
          case recurrent: NeuronRecurrent =>
            recurrent.inputs ++ recurrent.recurrentInputs
          case n: Neuron =>
            n.inputs
        }).zip(neuron.weights).foreach { case (id, weight) =>
          errors(id) += weight * errors(neuron.id)
          if (!isBe(id))
            set += id
        }
        for (i <- neuron.inputs.indices)
          neuron.weights(i) +=
            errors(neuron.id) * speed * ns.neurons(neuron.inputs(i)).result()
        neuron match {
          case recurrent: NeuronRecurrent =>
            for (i <- neuron.inputs.length until (neuron.weights.length - 1))
              neuron.weights(i) += errors(neuron.id) * speed *
                ns.neurons(recurrent.recurrentInputs(i - recurrent.inputs.length)).asInstanceOf[NeuronRecurrent].oldResult
          case _ =>
        }
        neuron.weights(neuron.weights.length - 1) += errors(neuron.id) * speed
      case _ =>
    }
  }
}
