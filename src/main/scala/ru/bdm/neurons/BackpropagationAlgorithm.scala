package ru.bdm.neurons

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class BackpropagationAlgorithm(val ns: NeuronSystem, var speed: Double = 0.5, var moment:Double = 0.3) {


  val visited: Array[Boolean] = new Array(ns.neurons.length)

  val errors: Array[Double] = new Array(ns.neurons.length)
  var nextNeurons: Set[Int] = Set.empty
  var error: Double = 1
  val deltaWeights:Array[Array[Double]] = new Array(ns.neurons.length)

  for(i <- deltaWeights.indices){
    deltaWeights(i) = Array.fill(ns.neurons(i).weights.length)(0d)
  }

  def teach(trainSet: Iterator[(Seq[Double], Seq[Double])], maxNumber:Int = Int.MaxValue, logIteration: Int = -1): Unit = {

    for (((input, answer), index) <- trainSet.zipWithIndex){
      if(index > maxNumber) {
        if(logIteration > 0) printLog(1, index)
        return
      }
      printLog(logIteration, index)
      teachOne(input, answer)
    }
  }

  private def printLog(logIteration: Int, index: Int): Unit = {
    if (logIteration > 0 && index % logIteration == 0)
      println(s"error=$error index=$index speed=$speed moment=$moment ns_inputs=${ns.inputs.length} ns_outputs=${ns.outputs.length}")
  }

  def teachOne(inputs: Seq[Double], rights: Seq[Double]): Unit = {
    ns.work(inputs)

    for (i <- visited.indices) {
      visited(i) = false
    }
    for (i <- errors.indices)
      errors(i) = 0
    error = 0

    ns.outputs.zip(rights).foreach { case (neuron, right) =>
      error += Math.abs(right - neuron.result())
      errors(neuron.id) = (right - neuron.result())
      nextNeurons += neuron.id
    }
    error = error / ns.outputs.length


    while (nextNeurons.nonEmpty) {
      val oldSet = nextNeurons

      nextNeurons = Set.empty
      oldSet foreach { id =>
          calculate(id)
      }
    }
  }

  private def calculate(id: Int): Unit = {
    ns.neurons(id) match {
      case neuron: Neuron if !visited(neuron.id) =>
        visited(neuron.id) = true
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
          if (!visited(id))
            nextNeurons += id
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
