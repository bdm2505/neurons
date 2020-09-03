package ru.bdm.neurons

trait Out {
  val id:Int
  def out:Double
  def update():Double = { out }
}

