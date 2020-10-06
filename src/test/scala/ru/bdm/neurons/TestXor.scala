package ru.bdm.neurons

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.Random

class TestXor extends AnyFlatSpec {

  "NeuronSystem" should "xor" in {
    val ns = NeuronSystem(Layer(2) * Layer(7) * Layer(7) * Layer(1))

    val random = new Random(seed = 13)
    ns.setRandomWeights(random)
    val alg = new BackpropagationAlgorithm(ns, 0.7, 0.9)
    val arr = Seq(
      Seq(0d, 0d) -> Seq(0d),
      Seq(1d, 0d) -> Seq(1d),
      Seq(0d, 1d) -> Seq(1d),
      Seq(1d, 1d) -> Seq(0d),
    )
    //  println(arr.map(a => Math.abs(ns.work(a._1).head - a._2.head)).sum / 4d)
    alg.teach(TrainSet(arr(random.nextInt(4))), maxNumber = 200000)
    arr.map(a => Math.abs(ns.work(a._1).head - a._2.head)).sum / 4d < 0.01 shouldEqual true
    //  println(arr.map(a => ns.work(a._1).head + " <-- " + a._2.head))

  }


}
