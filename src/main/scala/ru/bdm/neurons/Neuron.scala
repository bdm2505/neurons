package ru.bdm.neurons

trait Neuron {

  def update(): Unit = {}
  def work():Double
  def id:Int
  def weights:Array[Double]
}



