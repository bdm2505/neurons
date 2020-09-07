package ru.bdm.neurons

case class NeuronModel(id: Int,
                       inputs: Seq[Int] = Seq.empty,
                       tag: NeuronTag.Type = NeuronTag.hidden,
                       func: Func.Type = Func.sigmoid,
                       recurrentInputs: Option[Seq[Int]] = None
                      ) {
  def create(neurons: Int => Neuron): Neuron = {
    if (recurrentInputs.isDefined) {
      println(s"recurrent neuron $id")
      new NeuronRecurrent(id, neurons, inputs.toArray, Func(func), recurrentInputs.get.toArray)
    } else {
      println(s"create neuron $id")
      new Neuron(id, neurons, inputs.toArray, Func(func))
    }
  }
}
