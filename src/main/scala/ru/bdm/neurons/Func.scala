package ru.bdm.neurons



object Func {

  type Type = String
  val sigmoid = "sigmoid"
  val linear = "linear"
  val stepped = "stepped"
  val tangent = "tangent"

  private val activate:Map[Type, Double => Double] = Map(
    sigmoid ->  (x => 1d / (1d + Math.pow(Math.E, -x))),
    tangent -> (x => 2d / (1d + Math.pow(Math.E, -2d * x)) - 1d),
    linear -> (x => x),
    stepped -> (x => if (x > 0) 1d else 0d)
  )
  private val deactivate:Map[Type, Double => Double] = Map(
    sigmoid -> (x => (1 - x) * x),
    linear -> (x => 1d),
    stepped -> (x => 0d)
  )


  def apply(func:Type): Double => Double = activate.apply(func)
  def derivative(func:Type): Double => Double = deactivate.apply(func)

}
