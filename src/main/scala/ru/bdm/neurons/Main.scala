package ru.bdm.neurons

import java.io.{File, PrintWriter}

import org.json4s._
import org.json4s.native.Serialization

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.{Source, StdIn}


object Main {

val LEN = 7
  def main(args: Array[String]): Unit = {
    implicit val formats = Serialization.formats(NoTypeHints)

    val alf = "ёйцукенгшщзхъфывапролджэячсмитьбю.,!? "
    val loadFile = "result.json"
    val s1 = Source.fromFile(loadFile)
    //val ns = Serialization.read[NeuronSystemWrite](s1.getLines().mkString(" ")).create()

   // val rec = Layer.recurrent(30, id => Seq(id, (id + 2) % 10))
    val rec = Layer(6)
    val l1 = Layer(LEN) * Layer(10) * rec  * Layer(alf.length)
    val l2 = l1 * l1 * l1
    println(l2)

    val ns = NeuronSystem(l2)
//    val ns = NeuronSystem.createStandardModel(Seq(alf.length, 10, alf.length))
    val bpa = new BackpropagationAlgorithm(ns, 0.9)

    def genInputs(in: Seq[Char]): Array[Double] = {
      val arr = Array.fill(alf.length)(.1)
      for (i <- in.indices) {
        arr(i) = alf.indexOf(in(i)).toDouble / alf.length
      }
      arr
    }

    def genOuts(char: Char): Array[Double] = {
      val arr = Array.fill(alf.length)(.1)
      arr(alf.indexOf(char)) = 1.0
      arr
    }

    def getOut(out: Seq[Double]): Char = {
      alf(out.indexOf(out.max))
    }

    val source = Source.fromFile("pushkin.txt")
    val reg = "[ .,]+".r
    var str = source.getLines().mkString(" ").toLowerCase.filter(alf.contains(_))
    str = reg.replaceAllIn(str, " ").slice(0, 10)
    println(str)
    var noEnd = true
    val future = Future {
      var iter = 0
      while (noEnd) {
        iter += 1
        val st_index = iter % (str.length - LEN - 1)
        val inputs = genInputs(str.slice(st_index, st_index + LEN))
        val oldRes = getOut(ns.work(inputs))
        bpa.teachOne(inputs,genOuts(str.charAt(st_index + LEN)))
        if(iter % 531 == 0)
          println(s"${str.slice(st_index, st_index + LEN)}(${str.charAt(st_index + LEN)}) -> ${getOut(ns.work(inputs))} old($oldRes) error=${bpa.error}")
      }
    }

    var in = StdIn.readLine()
    noEnd = false
    Await.ready(future, Duration.Inf)
    val wr = new PrintWriter(new File(loadFile))

    wr.write(Serialization.writePretty(ns.write()))
    wr.close()
    while (in != "exit") {
      print("enter start words 5 symbols:")
      in = StdIn.readLine()
      for (_ <- 1 to 20)
        in += getOut(ns.work(genInputs(in.slice(in.length - 5, in.length))))
      println(s"'$in'")
    }
  }

  def printTime[T](name: String = "no name")(fun: => T): T = {
    val st = System.currentTimeMillis()
    val t = fun
    println(s"$name end in ${(System.currentTimeMillis() - st) / 1000d} sec")
    t
  }
}
