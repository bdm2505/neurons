package ru.bdm.neurons

case class NeuronModel(id: Int,
                       inputs: Seq[Int] = Seq.empty,
                       func: Func.Type = Func.sigmoid,
                       recurrentInputs: Option[Seq[Int]] = None
                      ) {
  def create(neurons: Int => Neuron): Neuron = {
    if (recurrentInputs.isDefined) {
      new NeuronRecurrent(id, neurons, inputs.toArray, Func(func), recurrentInputs.get.toArray)
    } else {
      new Neuron(id, neurons, inputs.toArray, Func(func))
    }
  }
}
