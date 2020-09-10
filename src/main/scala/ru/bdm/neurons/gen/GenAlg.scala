package ru.bdm.neurons.gen

import scala.util.Random

abstract class GenAlg[CHR](val numberChroms: Int = 100, val percentBest: Double = 0.1, val percentMerge: Double = 0.8) {

  var chroms: Seq[CHR] = 1 to numberChroms map(_ => create())
  var bestResult = Double.MinValue

  def create(): CHR

  def merge(first: CHR, second: CHR): CHR

  def appraisal(chroms: Seq[CHR]): Seq[Double]

  def learnOne(): Unit = {
    val appr = appraisal(chroms).zipWithIndex.sortBy(_._1)
    bestResult = appr.head._1
    val sorted = appr.map { case (_, index) => chroms(index) }
    val bestNumber = (numberChroms * percentBest) toInt
    val best = sorted.slice(0, bestNumber)
    val forMerge = sorted.slice(bestNumber, bestNumber * 2)
    val mergeNumber = (numberChroms * percentMerge) toInt
    val merges = 1 to mergeNumber map { _ =>
      merge(best(Random.nextInt(best.length)), forMerge(Random.nextInt(forMerge.length)))
    }
    val newNumber = numberChroms - mergeNumber - bestNumber;
    val creaters = 1 to newNumber map(_ => create())
    chroms = best ++ merges ++ creaters
  }

}
