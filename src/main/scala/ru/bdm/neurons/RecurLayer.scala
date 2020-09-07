package ru.bdm.neurons

object RecurLayer {
  def apply(in: Int, layer: Int, out: Int): Layer = {
    val inputs = 0 until in map (id => NeuronModel(id, Seq((id + in - 1) % in), tag = NeuronTag.input))

    val ss = (in) until (in + layer) map { id =>
      NeuronModel(id, Seq((id + layer - 1) % layer + in) ++ (0 until in))
    }

    val end = (in + layer) until (in + layer + out) map (id =>
      NeuronModel(id, Seq((id + out - 1) % out + in + layer) ++ (in until (layer + in)), NeuronTag.output))

    new Layer(inputs ++ ss ++ end)
  }
}
