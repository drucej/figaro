package com.cra.figaro.algorithm.adfactored

import com.cra.figaro.algorithm.factored.{ADVariableElimination, VariableElimination}
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{CPD, If}


/**
  * @author aoconnor
  */
class ModelV2(numTimesteps: Int, parameterValue: Double) {
  val universe = Universe.createNew
  val pJohnCallGivenAlarm = Constant(parameterValue)
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

    // builds a model for a particular parameter value
    def createModel(parameterValue: Double) = {
      Universe.createNew()
      val model = new ModelV2(numTimesteps, parameterValue)
      model.johnCalls(1).observe(true)
      model
    }

    val parameterValue = 0.7
    val targetValue = true

    for (i <- 0 until numTimesteps) {
      // builds model and serves up the inference target
      def createPerturbedModel(parameterValue: Double) = {
        val model = createModel(parameterValue)
        model.johnCalls(i)
      }
      val expectedDerivative = computeDerivativeNumerically(parameterValue, createPerturbedModel, targetValue)
      println(s"Expected derivative for time step i=$i when johnCalls(i) is $targetValue and parameter is $parameterValue = $expectedDerivative")

      val model = createModel(parameterValue)
      val alg = ADVariableElimination.debugged(model.pJohnCallGivenAlarm, model.johnCalls(i))
      alg.start()
      val (prob, deriv) = alg.getProbabilityAndDerivative(model.johnCalls(i), targetValue)
      println("time step : " + i + " , probability: " + prob + " , derivitive : " + deriv)
      assert(Math.abs(deriv - expectedDerivative) < 5e-2)
      alg.kill
    }
  }

  def computeDerivativeNumerically[T](parameterValue: Double, modelGenerator: Double => Element[T], targetValue: T, figaroSeedHack: () => Unit = () => ()) = {
    val delta = 1e-6
    val probs = for (param <- List(parameterValue - delta, parameterValue + delta)) yield {
      Universe.createNew()
      figaroSeedHack()
      val target = modelGenerator(param)
      val alg = VariableElimination(target)
      alg.start()
      val prob = alg.probability(target, targetValue)
      alg.kill()
      prob
    }
    (probs.last - probs.head) / (2.0 * delta)
  }
}
