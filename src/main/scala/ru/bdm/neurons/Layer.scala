package ru.bdm.neurons

class Layer(val neurons: IndexedSeq[NeuronModel] = IndexedSeq.empty, name: String = "") extends IndexedSeq[NeuronModel] {

  val inputs: IndexedSeq[NeuronModel] = neurons.filter(_.tag == NeuronTag.input)

  val outputs: IndexedSeq[NeuronModel] = neurons.filter(_.tag == NeuronTag.output)

  def connectAllInputsTo(layer: Layer): Layer = {
    val out = outputs.map(n => n.copy(tag = NeuronTag.layer))
    val outIds = outputs.map(_.id)
    val in = layer.inputs.map(n => n.copy(inputs = outIds ++ n.inputs, tag = NeuronTag.layer))
    createLayer(layer, out, in)
  }

  private def createLayer(layer: Layer, out: IndexedSeq[NeuronModel], in: IndexedSeq[NeuronModel]) = {
    new Layer(neurons.filter(_.tag != NeuronTag.output) ++ out ++ layer.neurons.filter(_.tag != NeuronTag.input) ++ in)
  }

  def connectInputsToEveryone(layer: Layer): Layer = {
    if (outputs.length != layer.inputs.length)
      throw new Exception("количество входов выходов не равны!")
    val out = outputs.map(n => n.copy(tag = NeuronTag.layer))
    val in = outputs.map(_.id).zip(layer.inputs).map { case (id, neuron) =>
      neuron.copy(inputs = neuron.inputs :+ id, tag = NeuronTag.layer)
    }
    createLayer(layer, out, in)
  }

  override def toString(): String = s"Layer(name=$name, in_size=${inputs.length}, out_size=${outputs.length})"

  override def apply(i: Int): NeuronModel = neurons(i)

  override def length: Int = neurons.length
}

