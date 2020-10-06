package ru.bdm.neurons

import org.scalatest._
import matchers.should.Matchers._
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.flatspec.AnyFlatSpec
import ru.bdm.neurons.Func

class TestNeurons extends AnyFlatSpec {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.00001)

  "Func" should "work sigmoid " in {
    val fun = Func("sigmoid")
    fun(1) shouldEqual 0.73106
    fun(0) shouldEqual 0.5
    fun(Double.MinValue) shouldEqual 0.0
  }

  "Func" should "sigmoid dis activate" in {
    val fun = Func.derivative("sigmoid")
    fun(1) shouldEqual 0.0
    fun(0) shouldEqual 0.0
    fun(-1) shouldEqual -2.0
  }
}