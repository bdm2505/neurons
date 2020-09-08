package ru.bdm.neurons

case class NeuronSystemWrite(model: Seq[NeuronModel], neurons: Seq[WeightsWrite], inputs:Seq[Int], outputs:Seq[Int]) {
  def create(): NeuronSystem = {
    val ns = NeuronSystem(new Layer(model.toIndexedSeq, inputs, outputs))
    neurons.foreach { neuron =>
      ns.neurons(neuron.id).setWeights(neuron.weights)
    }
    ns
  }
}

