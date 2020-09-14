package ru.bdm.neurons.gen

import scala.util.Random

abstract class GeneticAlgorithm[CHR](val numberChromosomes: Int = 100,
                                     val percentBest: Double = 0.1,
                                     val percentMerge: Double = 0.8) {

  var chromosomes: Seq[CHR] = 1 to numberChromosomes map(_ => create())
  var bestResult: Double = Double.MinValue

  def create(): CHR

  def merge(first: CHR, second: CHR): CHR

  def appraisal(chromosomes: Seq[CHR]): Seq[Double]

  def learnOne(): Unit = {
    val counted = appraisal(chromosomes).zipWithIndex.sortBy(_._1)
    bestResult = counted.head._1
    val sorted = counted.map { case (_, index) => chromosomes(index) }
    val bestNumber = (numberChromosomes * percentBest) toInt
    val best = sorted.slice(0, bestNumber)
    val forMerge = sorted.slice(bestNumber, bestNumber * 2)
    val mergeNumber = (numberChromosomes * percentMerge) toInt
    val merges = 1 to mergeNumber map { _ =>
      merge(best(Random.nextInt(best.length)), forMerge(Random.nextInt(forMerge.length)))
    }
    val newNumber = numberChromosomes - mergeNumber - bestNumber
    val creating = 1 to newNumber map(_ => create())
    chromosomes = best ++ merges ++ creating
  }

}
