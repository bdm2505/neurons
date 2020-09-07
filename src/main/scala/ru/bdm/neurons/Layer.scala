package ru.bdm.neurons

class Layer(val neurons: IndexedSeq[NeuronModel] = IndexedSeq.empty) extends IndexedSeq[NeuronModel] {

  var ids:Int = neurons.length - 1
  def nextId:Int = {
    ids+=1
    ids
  }


  val inputs: IndexedSeq[NeuronModel] = neurons.filter(_.tag == NeuronTag.input)

  val outputs: IndexedSeq[NeuronModel] = neurons.filter(_.tag == NeuronTag.output)
  def renameIds(start:Int): Layer = {
    new Layer(neurons.map(n => n.copy(n.id + start, n.inputs.map(i => i + start))))
  }

  def connectAllInputsTo(layer: Layer): Layer = {
    val me = renameIds(layer.length)
    val out = me.outputs.map(n => n.copy(tag = NeuronTag.layer))
    val outIds = me.outputs.map(_.id)
    val in = layer.inputs.map(n => n.copy(inputs = outIds ++ n.inputs, tag = NeuronTag.layer))
    createLayer(me, layer, out, in)
  }

  private def createLayer(me:Layer, layer: Layer, out: IndexedSeq[NeuronModel], in: IndexedSeq[NeuronModel]) = {
    new Layer(me.neurons.filter(_.tag != NeuronTag.output) ++ out ++ layer.neurons.filter(_.tag != NeuronTag.input) ++ in)
  }

  def connectInputsToEveryone(layer: Layer): Layer = {
    if (outputs.length != layer.inputs.length)
      throw new Exception("количество входов выходов не равны!")
    val me = renameIds(layer.length)
    val out = me.outputs.map(n => n.copy(tag = NeuronTag.layer))
    val in = me.outputs.map(_.id).zip(layer.inputs).map { case (id, neuron) =>
      neuron.copy(inputs = neuron.inputs :+ id, tag = NeuronTag.layer)
    }
    createLayer(me, layer, out, in)
  }

  override def toString(): String = s"Layer(in_size=${inputs.length}, out_size=${outputs.length} ${super.toString})"

  override def apply(i: Int): NeuronModel = neurons(i)

  override def length: Int = neurons.length
}
object Layer {
  def empty(startId:Int, number:Int) =
    new Layer(startId until(startId + number) map (id => NeuronModel(id)))

}

