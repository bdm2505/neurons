package ru.bdm.neurons



object Func {

  type Type = String
  val sigmoid = "sigmoid"
  val linear = "linear"

  private val activate:Map[Type, Double => Double] = Map(
    sigmoid ->  (x => 1.0 / (1 + Math.pow(Math.E, -x))),
    linear -> (x => x)
  )
  private val deactivate:Map[Type, Double => Double] = Map(
    sigmoid -> (x => (1 - x) * x),
    linear -> (x => 1d)
  )


  def apply(func:Type): Double => Double = activate.apply(func)
  def derivative(func:Type): Double => Double = deactivate.apply(func)

}
