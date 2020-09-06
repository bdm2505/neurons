package ru.bdm.neurons

case class NeuronSystemWrite(model: Seq[NeuronModel], neurons: Seq[WeightsWrite]) {
  def create(): NeuronSystem = {
    val ns = NeuronSystem.create(model)
    neurons.foreach { neuron =>
      val arr = ns.neurons(neuron.id).weights
      for (i <- arr.indices)
        arr(i) = neuron.weights(i)
    }
    ns
  }
}

