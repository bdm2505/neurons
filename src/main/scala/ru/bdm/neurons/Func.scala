package ru.bdm.neurons

object Func {

  type Type = String
  val sigmoid = "sigmoid"

  private val map:Map[Type, Double => Double] = Map(
    sigmoid -> sigmoidFun
  )


  def apply(func:Type): Double => Double = map.apply(func)

  private def sigmoidFun(x:Double): Double = 1.0 / (1 + Math.pow(Math.E, -x) )
}
