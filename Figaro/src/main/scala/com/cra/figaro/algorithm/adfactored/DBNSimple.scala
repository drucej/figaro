package com.cra.figaro.algorithm.adfactored

import com.cra.figaro.algorithm.factored.{ADVariableElimination, VariableElimination}
import com.cra.figaro.language._
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.library.compound.If
import com.cra.figaro.test.algorithm.adfactored.simpleTest.{Alert, pThreat}


/**
  * @author aoconnor
  */
class ModelV2(numTimesteps: Int) {

  val universe = Universe.createNew


  val pJohnCallGivenAlarm = Constant(0.7)


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
//  val Queries: Array[Double] = Array.emptyDoubleArray(numTimesteps)



  //transition function
  for (i <- 1 until numTimesteps) {

    burglaries(i) = If(burglaries(i-1), Flip(0.05), Flip(0.01))
    earthquakes(i) = If(earthquakes(i-1), Flip(0.000001), Flip(0.0001))
    alarms(i) = CPD(burglaries(i), earthquakes(i),
      (false, false) -> Flip(0.001),
      (false, true) -> Flip(0.1),
      (true, false) -> Flip(0.9),
      (true, true) -> Flip(0.99))
    johnCalls(i) = CPD(alarms(i),
      false -> Flip(0.01),
      true -> Flip(0.7))

  }

}

object DBNSimple {

  def main(args: Array[String]) {

    val numTimesteps = 2
    val model = new ModelV2(numTimesteps)

    //    val targets = model.alarms
    //    val alg = VariableElimination(targets: _*)
    //    alg.start
    //    alg.stop

    val pJohnCallGivenAlarm = Constant(0.7)
//    val targets = model.johnCalls.last

    model.johnCalls(1).observe(true)

    for (i <- 0 until numTimesteps) {
      //      val targetProbability = alg.probability(model.alarms(i), true)
      val alg = ADVariableElimination.debugged(model.pJohnCallGivenAlarm, model.johnCalls(i))
      alg.start()
      alg.stop()
      val (prob, deriv) = alg.getProbabilityAndDerivative(model.johnCalls(i), true)
      println("time step : " + i + " , probability: " + prob + " , derivitive : " + deriv)

      alg.kill

    }

  }

}