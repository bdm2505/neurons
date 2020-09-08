package ru.bdm.neurons

object RecurLayer {
  def apply(in: Int, hidden: Int, out: Int): Layer = {
    val inputs = 0 until in map (id => NeuronModel(id, Seq()))

    val ss = (in) until (in + hidden) map { id =>
      NeuronModel(id, 0 until in, recurrentInputs = Some(Seq(id)))
    }
    val ss1 = (in + hidden) until (in + hidden * 2) map { id =>
      NeuronModel(id, in until (in + hidden), recurrentInputs = Some(Seq(id)))
    }

    val end = (in + hidden * 2) until (in + hidden * 2 + out) map (id =>
      NeuronModel(id, in until (hidden + in), NeuronTag.output))

    new Layer(inputs ++ ss ++ ss1 ++ end)
  }
}
