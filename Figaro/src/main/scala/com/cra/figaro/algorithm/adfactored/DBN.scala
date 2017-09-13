package com.cra.figaro.algorithm.adfactored


import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.library.compound.If


/**
  * @author aoconnor
  */
class Model(numTimesteps: Int) {

  val universe = Universe.createNew



  private val burglary = Flip(0.01)

  private val earthquake = Flip(0.0001)

  private val alarm = CPD(burglary, earthquake,
    (false, false) -> Flip(0.001),
    (false, true) -> Flip(0.1),
    (true, false) -> Flip(0.9),
    (true, true) -> Flip(0.99))

  private val johnCall = CPD(alarm,
    false -> Flip(0.01),
    true -> Flip(0.7))

  val burglaries: Array[Element[Boolean]] = Array.fill(numTimesteps)(burglary)
  val earthquakes: Array[Element[Boolean]] = Array.fill(numTimesteps)(earthquake)
  val johnCalls: Array[Element[Boolean]] = Array.fill(numTimesteps)(johnCall)
  val alarms: Array[Element[Boolean]] = Array.fill(numTimesteps)(alarm)


  //transition function
  for (i <- 1 until numTimesteps) {

    burglaries(i) = If(burglaries(i-1), Flip(0.05), Flip(0.01))
    earthquakes(i) = If(earthquakes(i-1), Flip(0.000001), Flip(0.0001))
    alarms(i) = If(alarms(i-1), Flip(0.5), CPD(burglaries(i), earthquakes(i),
      (false, false) -> Flip(0.001),
      (false, true) -> Flip(0.1),
      (true, false) -> Flip(0.9),
      (true, true) -> Flip(0.99)))
    johnCalls(i) = CPD(alarms(i-1),
      false -> Flip(0.01),
      true -> Flip(0.7))

  }

}

object LocalInferenceModel {

  def main(args: Array[String]) {

    val numTimesteps = 10
    val model = new Model(numTimesteps)

      val targets = model.alarms
      val alg = VariableElimination(targets: _*)
      alg.start
      alg.stop

      for (i <- 0 until numTimesteps) {
        val targetProbability = alg.probability(model.alarms(i), true)
        println(targetProbability)
      }

      alg.kill
  }

}