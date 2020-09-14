package ru.bdm.neurons.gen

import ru.bdm.neurons.{Layer, NeuronSystem, NeuronSystemWrite}

import scala.util.Random
import scala.collection.parallel.CollectionConverters._

class GeneticAlgorithmForNeuron(val layer: Layer,
                                fun:NeuronSystem => Double,
                                val parallel:Boolean = false,
                                numberChromosomes: Int = 100,
                                percentBest: Double = 0.1,
                                percentMerge: Double = 0.8
                               ) extends GeneticAlgorithm[NeuronSystem](numberChromosomes, percentBest, percentMerge) {

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

  override def appraisal(chromosomes: Seq[NeuronSystem]): Seq[Double] = {
    if(parallel)
      chromosomes.par.map(fun(_)).seq
    else
      chromosomes.map(fun(_))
  }
}
