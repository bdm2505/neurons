package ru.bdm.neurons

case class NeuronSystemWrite(model: Layer, neurons: Seq[WeightsWrite]) {
  def create(): NeuronSystem = {
    val ns = NeuronSystem(model)
    neurons.foreach { neuron =>
      ns.neurons(neuron.id).setWeights(neuron.weights)
    }
    ns
  }
}

