package ru.bdm.neurons

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class BackpropagationAlgorithm(val ns: NeuronSystem, var speed: Double = 0.5, var moment:Double = 0.3) {

  val isBe: Array[Boolean] = new Array(ns.neurons.length)

  val errors: Array[Double] = new Array(ns.neurons.length)
  var set: Set[Int] = Set.empty
  var sumError: Double = 0
  val deltaWeights:Array[Array[Double]] = new Array(ns.neurons.length)

  for(i <- deltaWeights.indices){
    deltaWeights(i) = Array.fill(ns.neurons(i).weights.length)(0d)
  }

  def teach(inputs: Seq[Double], rights: Seq[Double], parallel: Boolean = false): Unit = {
    ns.work(inputs)

    for (i <- isBe.indices) {
      isBe(i) = false
    }
    for (i <- errors.indices)
      errors(i) = 0
    sumError = 0

    ns.outputs.zip(rights).foreach { case (neuron, right) =>
      sumError += Math.pow(right - neuron.result(), 2)
      errors(neuron.id) = (right - neuron.result())
      set += neuron.id
    }
    sumError = Math.sqrt(sumError / ns.outputs.length)


    while (set.nonEmpty) {
      val oldSet = set

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
//        print(s"error active id=${neuron.id} ${neuron.derivative(neuron.result())} old error=${errors(neuron.id)} new=")
        errors(neuron.id) *= neuron.derivative(neuron.result())
//        println(errors(neuron.id))

        (neuron match {
          case recurrent: NeuronRecurrent =>
            recurrent.inputs ++ recurrent.recurrentInputs
          case n: Neuron =>
            n.inputs
        }).zip(neuron.weights).foreach { case (id, weight) =>
       //   println(s"update err $id += ${weight * errors(neuron.id)} ($weight * ${errors(neuron.id)})")
          errors(id) += weight * errors(neuron.id)
          if (!isBe(id))
            set += id
        }
        val results = neuron match {
          case recurrent: NeuronRecurrent =>
            neuron.inputs.map(ns.neurons(_).result()) ++
              recurrent.recurrentInputs.map(ns.neurons(_).asInstanceOf[NeuronRecurrent].oldResult)
          case neuron: Neuron =>
            neuron.inputs.map(ns.neurons(_).result())
        }
        updateWeights(neuron, results)
        if(neuron.inputs.isEmpty)
          neuron.weights(neuron.weights.length - 1) += errors(neuron.id) * speed
      case _ =>
    }
  }

  def updateWeights(neuron:Neuron, results: Seq[Double]): Unit = {
    for(i <- results.indices) {
      val dw = speed * errors(neuron.id) * results(i)
      neuron.weights(i) += dw + moment * deltaWeights(neuron.id)(i)
      deltaWeights(neuron.id)(i) = dw
    }
  }
}
