package ru.bdm.neurons.gen

abstract class GenAlg[CHR](val numberChroms:Int = 100, val percentBest:Double = 0.1, val percentMerge:Double = 0.8) {

  val chroms:Array[CHR] = new Array[CHR](numberChroms)
  def create():CHR
  def merge(first:CHR, second:CHR):CHR
  def appraisal(chroms:Seq[CHR]): Seq[Double]
  def learn(): Unit = {
    for(i <- chroms.indices)
      chroms(i) = create()
    val appr = appraisal(chroms).zipWithIndex.sortBy(_._1)
    val best = appr.slice(0, numberChroms * percentBest toInt)


  }
}
