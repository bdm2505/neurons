package ru.bdm.neurons

case class NeuronModel(
                       inputs: Seq[Int] = Seq.empty,
                       func: Func.Type = Func.sigmoid,
                       recurrentInputs: Option[Seq[Int]] = None
                      ) {
  def create(id:Int, neurons: Int => Neuron): Neuron = {
    if (recurrentInputs.isDefined) {
      new NeuronRecurrent(id, neurons, inputs.toArray, Func(func), recurrentInputs.get.toArray)
    } else {
      new Neuron(id, neurons, inputs.toArray, Func(func))
    }
  }
}
