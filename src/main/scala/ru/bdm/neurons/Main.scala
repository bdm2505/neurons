package ru.bdm.neurons

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write, writePretty}

import scala.io.{Source, StdIn}
import scala.util.matching.Regex


object Main {



  def main(args: Array[String]): Unit = {
    implicit val formats = Serialization.formats(NoTypeHints)

    val alf = "ёйцукенгшщзхъфывапролджэячсмитьбю.,!? "
    val ns = NeuronSystem.createStandardModel(Seq(alf.length * 5,10,5,5,5, alf.length))
    val bpa = new BackpropagationAlgorithm(ns)

    def genInputs(in:Seq[Char]): Array[Double] = {
      val arr = Array.fill(alf.length * 5)(.1)
      for(i <- in.indices){
        arr(alf.indexOf(in(i)) + i * alf.length) = 1.0
      }
      arr
    }
    def genOuts(char: Char): Array[Double] = {
      val arr = Array.fill(alf.length)(.1)
      arr(alf.indexOf(char)) = 1.0
      arr
    }
    def getOut(out:Seq[Double]): Char = {
      alf(out.indexOf(out.max))
    }
    val source = Source.fromFile("pushkin.txt")
    val reg = "[ .,]+".r
    var str = source.getLines().mkString(" ").toLowerCase.filter(alf.contains(_))
    str = reg.replaceAllIn(str, " ").slice(0, 1000)
    println(str)
    printTime("work") {
      for (index <- 0 until (str.length - 5)) {
        bpa.teach(genInputs(str.slice(index, index + 5)), genOuts(str.charAt(index + 5)), 0.7)
        if (index % 100 == 0)
          println(bpa.sumError + " " + index.toFloat / str.length * 100 + "%")
      }
    }


    var in = StdIn.readLine()

    for(_ <- 1 to 20)
      in += getOut(ns.work(genInputs(in.slice(in.length - 5, in.length))))

    println(in)
  }

  def printTime[T](name:String = "no name")(fun: => T): T = {
    val st = System.currentTimeMillis()
    val t = fun
    println(s"$name end in ${(System.currentTimeMillis() - st) / 1000d} sec")
    t
  }
}
