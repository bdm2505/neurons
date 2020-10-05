package ru.bdm.neurons

import scala.util.Random

object MainTest extends App {

  val ns = NeuronSystem(Layer(2) * Layer(2) * Layer(1))
  ns.setWeights(Seq(Seq(0.5), Seq(0.5), Seq(0.5,0.5,0.5), Seq(0.5, 0.2, 0.7), Seq(0.5, 0.2, 0.1)))
  println(Func(Func.sigmoid)(0.5))
  println(ns)
  println(ns.work(Seq(0d,1d)))
  println(ns.neurons.map(n =>n.id + "-> "+ n.result()).mkString("(", ", ", ")"))

  val bpa = new BackpropagationAlgorithm(ns, 0.7)

  val a = new TrainSet {
    override def hasNext: Boolean = true
    override def next(): (Seq[Double], Seq[Double]) = Seq(1d,0d) -> Seq(1d)
  }
  bpa.teach(a, 100)

  println(ns.work(Seq(0d,1d)))
  println(ns.neurons.map(_.result()).mkString("(", ", ", ")"))

  bpa.teachOne(Seq(0d,1d), Seq(1d))
  println(bpa.errors.mkString("Error (", ", ", ")"))
  println(ns)
  println(ns.work(Seq(1d,0d)))
  println(ns.neurons.map(_.result()).mkString("(", ", ", ")"))


}
