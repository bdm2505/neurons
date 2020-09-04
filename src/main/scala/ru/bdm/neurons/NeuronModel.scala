package ru.bdm.neurons

case class NeuronModel(id:Int, inputs:Seq[Int] = Seq.empty, tag:NeuronTag.Type = NeuronTag.layer, func: Func.Type = Func.sigmoid)
