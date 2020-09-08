package ru.bdm.neurons

class Layer(val neurons: IndexedSeq[NeuronModel] = IndexedSeq.empty,
            val inputs:Seq[Int] = Seq.empty,
            val outputs:Seq[Int] = Seq.empty) extends IndexedSeq[NeuronModel] {

  var ids: Int = neurons.length - 1

  def nextId: Int = {
    ids += 1
    ids
  }


  def renameIds(start: Int): Layer = {
    new Layer(
      neurons.map(n => n.copy(n.id + start, n.inputs.map(_ + start), recurrentInputs = n.recurrentInputs.map(_.map(_ + start)))),
      inputs.map(_ + start),
      outputs.map(_ + start)
    )
  }

  def *(layer: Layer): Layer = {
    val newLayer = layer.renameIds(this.length)
    val newInput = newLayer.inputs.map(id => newLayer.find(_.id == id).get).map(n => n.copy(inputs = outputs ++ n.inputs))
    val rest = newLayer.filterNot(n => newLayer.inputs.contains(n.id))
    new Layer(this ++ newInput ++ rest, inputs, newLayer.outputs)
  }

  def +(layer: Layer): Layer = {
    if (outputs.length != layer.inputs.length)
      throw new Exception("количество входов выходов не равны!")
    val newLayer = layer.renameIds(this.length)
    val newInput = newLayer.inputs.map(id => newLayer.find(_.id == id).get).zip(outputs)
      .map{ case (n, out) => n.copy(inputs = n.inputs :+ out)}
    val rest = newLayer.filterNot(n => newLayer.inputs.contains(n.id))
    new Layer(this ++ newInput ++ rest, inputs, newLayer.outputs)
  }

  override def toString(): String = s"Layer(in_size=${inputs.length}, out_size=${outputs.length} " +
    s"hidden_size=${neurons.length - inputs.length - outputs.length} ${super.toString})"

  override def apply(i: Int): NeuronModel = neurons(i)

  override def length: Int = neurons.length
}

object Layer {
  def apply(number: Int) =
    new Layer(0 until (number) map (id => NeuronModel(id)), inputs = 0 until number, outputs = 0 until number)

  def apply(layers: Layer*): Layer = {
    var start = -layers.head.length
    new Layer(layers.flatMap { l =>
      start += l.length
      l.renameIds(start)
    }.toIndexedSeq)
  }
  def seq(model:Seq[NeuronModel]) = {
    new Layer(model.toIndexedSeq, model.indices, model.indices)
  }

  def recurrent(number: Int, recurrentInputs: Int => Seq[Int]) =
    new Layer(0 until (number) map (id => NeuronModel(id, recurrentInputs = Some(recurrentInputs(id)))), inputs = 0 until number, outputs = 0 until number)

  def model(model:Seq[Int]): Layer = {
    var layers = Layer(model.head)
    for(num <- model.slice(1, model.length - 1)){
      layers *= Layer(num)
    }
    layers * Layer(model.last)
  }
}

