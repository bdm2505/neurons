package ru.bdm.neurons


case class Layer(
                  neurons: Map[Int, NeuronModel],
                  inputs: Seq[Int],
                  outputs: Seq[Int]
                ) {

  def find(key: Int): Option[NeuronModel] = neurons.get(key)

  def size: Int = neurons.size

  val inputNeurons: Map[Int, NeuronModel] = inputs.map(i => i -> neurons(i)).toMap
  val outputNeurons: Map[Int, NeuronModel] = outputs.map(i => i -> neurons(i)).toMap

  private def renameIds(start: Int): Layer = {
    new Layer(
      neurons.map { case (id, neuron) =>
        (id + start) -> neuron.copy(neuron.inputs.map(_ + start), recurrentInputs = neuron.recurrentInputs.map(_.map(_ + start)))
      },
      inputs.map(_ + start),
      outputs.map(_ + start)
    )
  }

  def *(layer: Layer, displacementNeuron: Boolean = true): Layer = {
    val newLayer = layer.renameIds(this.size + (if (displacementNeuron) 1 else 0))
    val newInput = newLayer.inputNeurons.map { case (id, neuron) =>
      id -> neuron.copy(inputs = outputs ++ neuron.inputs ++ (if (displacementNeuron) Seq(this.size) else Seq.empty))
    }
    new Layer(newLayer.neurons ++ neurons ++ newInput ++ (if (displacementNeuron) Map(this.size -> NeuronModel()) else Map.empty), inputs, newLayer.outputs)
  }

  def +(layer: Layer): Layer = {
    if (outputs.length != layer.inputs.length)
      throw new Exception("количество входов выходов не равны!")
    val newLayer = layer.renameIds(this.size)
    val newInput = newLayer.inputNeurons.zip(outputs)
      .map { case ((id, n), out) => id -> n.copy(inputs = n.inputs :+ out) }

    new Layer(newLayer.neurons ++ neurons ++ newInput, inputs, newLayer.outputs)
  }

  def |(layer: Layer): Layer = {
    val newLayer = layer.renameIds(size)
    new Layer(
      neurons ++ newLayer.neurons,
      inputs ++ newLayer.inputs,
      outputs ++ newLayer.outputs
    )
  }

  override def toString: String = s"Layer(in_size=${inputs.length}, out_size=${outputs.length} " +
    s"hidden_size=${neurons.size - inputs.length - outputs.length} ${neurons.mkString("(",", ", ")")})"

}

object Layer {
  def apply(number: Int, func: Func.Type = Func.sigmoid): Layer = {
    new Layer((0 until number map (id => id -> NeuronModel(func = func))).toMap, inputs = 0 until number, outputs = 0 until number)
  }

  def notInput(number: Int, func: Func.Type = Func.sigmoid): Layer =
    new Layer((0 until number map (id => id -> NeuronModel(func = func))).toMap, inputs = Seq.empty, outputs = 0 until number)

  def recurrent(number: Int, recurrentInputs: Int => Seq[Int], func: Func.Type = Func.sigmoid) =
    new Layer((0 until number map (id => id -> NeuronModel(recurrentInputs = Some(recurrentInputs(id)), func = func))).toMap, inputs = 0 until number, outputs = 0 until number)

  def model(model: Seq[Int], func: Func.Type = Func.sigmoid, displacementNeuron: Boolean = true): Layer = {
    model.tail.foldLeft(Layer(model.head, func)){ case (layer, num) =>
      Layer(num, func) * (layer, displacementNeuron)
    }
  }
}

