package ru.bdm.neurons

case class NeuronWrite(id:Int, inputs:Seq[Int], weights:Seq[Double]){
  def create(toOut:Int ⇒ Out): Neuron = {
    new Neuron(inputs map toOut, id, weights.toArray)
  }
}

case class NeuronSystemWrite(inputs:Seq[InputNeuronWrite], neurons:Seq[NeuronWrite]) {
  def create: NeuronSystem = {
    val resInputs = inputs.map(_.create)
    var outs: Map[Int, Out] = resInputs.map(i ⇒ i.id → i).toMap
    def check(neuron: NeuronWrite):Boolean = {
      for (id ← neuron.inputs)
        if (!outs.contains(id))
          return false
      true
    }
    var used:Set[Int] = Set.empty
    var end = false
    while (!end) {
      end = true
      for (neuron ← neurons)
        if (!outs.contains(neuron.id) && check(neuron)) {
          used ++= neuron.inputs
          outs += neuron.id → neuron.create(outs.apply)
          end = false
        }
    }
    val res:Seq[Out] = (outs.keySet -- used).map(outs.apply).toSeq
    new NeuronSystem(resInputs, res)
  }

}

case class InputNeuronWrite(id:Int) {
  def create: InputNeuron = InputNeuron(id = id)
}
