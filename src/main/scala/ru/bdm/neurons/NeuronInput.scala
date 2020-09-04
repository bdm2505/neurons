package ru.bdm.neurons

class NeuronInput(override val id: Int, var value: Double = 0) extends Neuron {
  override def work(): Double = value

  override def weights: Array[Double] = Array.empty
}

