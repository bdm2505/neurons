package ru.bdm.neurons

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TestLayer extends AnyFlatSpec {

  "Layer" should "correctly * next layer" in {
    Layer(2) * Layer(1) shouldEqual
      new Layer(Map(0 -> NeuronModel(), 1 -> NeuronModel(), 2 -> NeuronModel(), 3 -> NeuronModel(inputs = Seq(0, 1, 2))), Seq(0, 1), Seq(3))

    Layer(1) * Layer(1) * Layer(1) shouldEqual
      new Layer(Map(0 -> NeuronModel(), 1 -> NeuronModel(), 2 -> NeuronModel(Seq(0, 1)), 3 -> NeuronModel(), 4 -> NeuronModel(Seq(2, 3))), Seq(0), Seq(4))

    (Layer(2) * Layer(4)) * Layer(3) shouldEqual
      Layer(2) * (Layer(4) * Layer(3))
  }

  "Layer" should "correctly + next layer" in {
    Layer(2) + Layer(2) shouldEqual
      new Layer(Map(0 -> NeuronModel(), 1 -> NeuronModel(), 2 -> NeuronModel(Seq(0)), 3 -> NeuronModel(Seq(1))), Seq(0, 1), Seq(2, 3))

    (Layer(2) + Layer(2)) + Layer(2) shouldEqual
      Layer(2) + (Layer(2) + Layer(2))
  }
  "Layer" should "correctly create layer from model" in {
    Layer.model(Seq(2, 3, 2)) shouldEqual
      Layer(2) * Layer(3) * Layer(2)
  }

  "Layer" should "correctly | next layer" in {
    Layer(2) | Layer(2) shouldEqual
      new Layer(Map(0 -> NeuronModel(), 1 -> NeuronModel(), 2 -> NeuronModel(), 3 -> NeuronModel()), Seq(0, 1, 2, 3), Seq(0, 1, 2, 3))

    (Layer(2) | Layer(4)) | Layer(3) shouldEqual
      Layer(2) | (Layer(4) | Layer(3))
  }
}
