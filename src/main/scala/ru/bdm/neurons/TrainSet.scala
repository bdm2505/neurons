package ru.bdm.neurons

trait TrainSet extends Iterator[(Seq[Double], Seq[Double])]{
  override def hasNext: Boolean = true

  override def next(): (Seq[Double], Seq[Double])
}

object TrainSet{
  def apply(fun: => (Seq[Double], Seq[Double])): TrainSet = () => fun
}
