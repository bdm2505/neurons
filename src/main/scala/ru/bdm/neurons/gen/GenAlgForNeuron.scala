package ru.bdm.neurons.gen

import ru.bdm.neurons.{Layer, NeuronSystem, NeuronSystemWrite}

import scala.util.Random

class GenAlgForNeuron(val layer: Layer, fun:NeuronSystem => Double) extends GenAlg[NeuronSystem] {

  val bestGeneFill = 0.6

  override def create(): NeuronSystem = NeuronSystem(layer)

  override def merge(first: NeuronSystem, second: NeuronSystem): NeuronSystem = {
    val newWeights = first.neurons.zip(second.neurons).map{ case (n1, n2) =>
      n1.weights.zip(n2.weights).map{ case (w1, w2) =>
        if (Random.nextDouble() < bestGeneFill) w1 else w2
      }
    }
    val ns = NeuronSystem(first.model)
    for(i <- ns.neurons.indices)
      ns.neurons(i).setWeights(newWeights(i))
    ns
  }

  override def appraisal(chroms: Seq[NeuronSystem]): Seq[Double] = {
    chroms.map(fun(_))
  }
}
