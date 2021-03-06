package ru.bdm.neurons

object MainXor extends App {

  var se = 0.0
  val NUM = 10
  for(_ <- 1 to NUM) {
    val ns = Main.printTime() {
      //NeuronSystem(RecurLayer(2,2,1))
      NeuronSystem(Layer(2) * Layer(2) * Layer(2) * Layer(1))
      //  NeuronSystem.readFromFile("main.xor.json")
    }
    println(ns.model)
    // ns.saveToFile("main.xor.json")

    val quest = Seq(Seq(1.0, 1.0) -> Seq(.0), Seq(.0, .0) -> Seq(.0), Seq(0d, 1d) -> Seq(1d), Seq(1d, 0d) -> Seq(1d))
    val bpa = new BackpropagationAlgorithm(ns, 0.9)
    println(ns.work(Seq(1d,0d)))
    println(ns.outputs.map(_.result()).mkString("Array(", ", ", ")"))
    teach(quest, bpa, ns)
  }


  private def teach(quest: Seq[(Seq[Double], Seq[Double])], bpa: BackpropagationAlgorithm, ns:NeuronSystem):Unit = {
    var old = 0.0
    for (i <- 1 to 1000000) {
      var sumE = 0d
      for ((input, answer) <- quest) {
        bpa.teachOne(input, answer)
        sumE += bpa.error
      }
      sumE /= 4
      //      println((bpa.sumError - old) * 100)

//      if(old != 0.0)
//        bpa.speed = if(dv > 0) Math.abs(1 - dv) * 10 else 0.7

      if (sumE < 0.003) {
        println(s"end in $i")
        print(s"$i: ${sumE} speed=${bpa.speed}    ")
        quest.foreach(e => print(ns.work(e._1).head + " (" + e._2.head + ")"))
        se += i
        println()
        return
      }
      if (i % 10000 == 0) {
        print(s"$i: ${sumE} speed=${bpa.speed}     ")
        quest.foreach(e => print(ns.work(e._1).head + " -> (" + e._2.head + "), "))
        println()
      }
    }
  }
  println("ser " + (se / NUM))

}