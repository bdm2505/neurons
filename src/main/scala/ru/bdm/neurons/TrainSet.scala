package ru.bdm.neurons

trait TrainSet extends Iterator[(Seq[Double], Seq[Double])]{
  override def hasNext: Boolean

  override def next(): (Seq[Double], Seq[Double])
}
