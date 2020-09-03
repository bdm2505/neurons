package ru.bdm.neurons

case class InputNeuron(var value:Double = 0, override val id:Int = Neuron.nextId()) extends Out {
  override def out: Double = value
  def write = InputNeuronWrite(id)
}
object N {
  def apply(value: Double): InputNeuron = InputNeuron(value)
}
