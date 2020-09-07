package ru.bdm.neurons

class NeuronRecurrent(id: Int,
                      neurons: Int => Neuron,
                      inputs: Array[Int] = Array.empty,
                      activate: Double => Double = Func(Func.sigmoid),
                      val recurrentInputs: Array[Int] = Array.empty,
                     ) extends Neuron(id, neurons, inputs, activate) {

  override val weights: Array[Double] = new Array(inputs.length + recurrentInputs.length + 1)

  var oldResult: Double = 0d

  override def work(in: Seq[Double]): Double = {
    super.work(in ++ recurrentInputs.map(neurons(_).asInstanceOf[NeuronRecurrent].oldResult))
  }

  override def update(): Unit = {
    oldResult = result()
    super.update()
  }
}
