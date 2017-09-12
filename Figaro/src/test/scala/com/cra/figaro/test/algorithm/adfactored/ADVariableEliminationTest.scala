/*
 * ADVariableElimination.scala
 * Auto differential variable elimination algorithm.
 *
 * Created By:      David Blumstein (dblumstein@cra.com)
 * Creation Date:   Aug 28, 2017
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.test.algorithm.adfactored

import com.cra.figaro.algorithm.factored.{ADVariableElimination, ParticleGenerator, VariableElimination}
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.compound.{CPD, If}
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class ADVariableEliminationTest extends WordSpec with Matchers {

  "ADVE" should {
    // this test works
    "get the same answers as Dan's example" in {
      def computeDerivative = {
        val parameterValue = 0.9
        val delta = 1e-6
        val probs = for (param <- List(parameterValue - delta, parameterValue + delta)) yield {
          Universe.createNew()
          val pThreat =  Constant(0.25)
          val pIndGivenThreat = Constant(param)
          val pIndGivenNotThreat = Constant(0.15)
          val threat = Flip(pThreat)
          val indicator =  Flip(If(threat,pIndGivenThreat,pIndGivenNotThreat))

          val inferenceTarget = indicator
          val inferenceTargetValue = true
          val alg = VariableElimination(inferenceTarget)
          alg.start()
          val prob = alg.probability(inferenceTarget, inferenceTargetValue)
          alg.kill()
          prob
        }
        (probs.last - probs.head) / (2.0 * delta)
      }
      val expectedDerivative = computeDerivative
      println(s"Expected derivative: $expectedDerivative")

      Universe.createNew() // it's critical that we clear the universe after computing expected derivative
      val pThreat =  Constant(0.25)
      val pIndGivenThreat = Constant(0.9)
      val pIndGivenNotThreat = Constant(0.15)
      val threat = Flip(pThreat)
      val indicator =  Flip(If(threat,pIndGivenThreat,pIndGivenNotThreat))

      val (prob, deriv) = runADVEandVE(pIndGivenThreat, indicator, true)
      checkValue(deriv, expectedDerivative)
    }

    // this one works
    "get the same answers as Dan's example with a no-op Apply" in {
      val pThreat =  Constant(0.25)
      val pIndGivenThreat = Constant(0.9)
      val pIndGivenNotThreat = Constant(0.15)
      val threat = Flip(pThreat)
      val indicator =  Flip(If(threat,Apply(pIndGivenThreat, (a: Double) => a),pIndGivenNotThreat))

      val expectedDerivative = 0.25
      val (prob, deriv) = runADVEandVE(pIndGivenThreat, indicator, true)
      checkValue(deriv, expectedDerivative)
    }

    // this one doesn't work
    "get the same answers as Dan's example with a no-op Select" in {
      val pThreat =  Constant(0.25)
      val pIndGivenThreat = Constant(0.9)
      val pIndGivenNotThreat = Constant(0.15)
      val threat = Flip(pThreat)
      val select = Select(1.0 -> pIndGivenThreat)
      val chn = Chain(select, (x:Constant[Double]) => x)
      val indicator =  Flip(If(threat, chn, pIndGivenNotThreat))

      val expectedDerivative = 0.25
      val (prob, deriv) = runADVEandVE(pIndGivenThreat, indicator, true)
      checkValue(deriv, expectedDerivative)
    }


    // We know this example is broken since the squaring is hidden from factor creation in the Apply
    "get answers in Dan's example with a squared value in the Apply" in {
      def computeDerivative = {
        val parameterValue = 0.9
        val delta = 1e-6
        val probs = for (param <- List(parameterValue - delta, parameterValue + delta)) yield {
          Universe.createNew()
          val pThreat =  Constant(0.25)
          val pIndGivenThreat = Constant(param)
          val pIndGivenNotThreat = Constant(0.15)
          val threat = Flip(pThreat)
          val indicator =  Flip(If(threat,Apply(pIndGivenThreat, (a: Double) => a * a),pIndGivenNotThreat))

          val inferenceTarget = indicator
          val inferenceTargetValue = true
          val alg = VariableElimination(inferenceTarget)
          alg.start()
          val prob = alg.probability(inferenceTarget, inferenceTargetValue)
          alg.kill()
          prob
        }
        (probs.last - probs.head) / (2.0 * delta)
      }
      val expectedDerivative = computeDerivative
      println(s"Expected derivative: $expectedDerivative")

      Universe.createNew() // it's critical that we clear the universe after computing expected derivative

      val pThreat =  Constant(0.25)
      val pIndGivenThreat = Constant(0.9)
      val pIndGivenNotThreat = Constant(0.15)
      val threat = Flip(pThreat)
      val indicator =  Flip(If(threat,Apply(pIndGivenThreat, (a: Double) => a * a),pIndGivenNotThreat))

      val (prob, deriv) = runADVEandVE(pIndGivenThreat, indicator, true)
      checkValue(deriv, expectedDerivative)
    }

    // this one should work
    "work on Jeff's Model3Node" in {
      val pThreat = 0.25
      val pIndGivenThreat = Constant(0.7)
      val pIndGivenNotThreat = Constant(0.15)
      val pAlertGivenInd = Constant(0.9)
      val pAlertGivenNotInd = Constant(0.12)

      val Threat = Flip(pThreat)
      val Ind = Flip(If(Threat, pIndGivenThreat, pIndGivenNotThreat))
      val Alert = Flip(If(Ind, pAlertGivenInd, pAlertGivenNotInd))

      val expectedDerivative = 0.25
      val (prob, deriv) = runADVEandVE(pAlertGivenInd, Alert, true)
      checkValue(deriv, expectedDerivative)
    }

    // this test does not work
    "work with a beta in a modified Jeff's Model3Node" in {
      // defaults here for both 15, which lead to about 3 samples per value
      // values of 50 don't seem to converge on a derivative
      // values of 100 give 25 samples but take a few minutes to run
      ParticleGenerator.defaultNumSamplesFromAtomics = 50
      ParticleGenerator.defaultMaxNumSamplesAtChain = 50

      def computeDerivative = {
        val parameterValue = 0.25
        val delta = 1e-6
        val probs = for (param <- List(parameterValue - delta, parameterValue + delta)) yield {
          // this is hacky. needs an edit to baseline figaro
          com.cra.figaro.util.setSeed(4444545)
          Universe.createNew()
          val pThreat = Constant(param)

          val aGivenThreat = Constant(4.0)
          val bGivenThreat = Constant(9.0)
          val aGivenNotThreat = Constant(6.0)
          val bGivenNotThreat = Constant(12.0)
          val betaGivenThreat = Beta(aGivenThreat, bGivenThreat)
          val betaGivenNotThreat = Beta(aGivenNotThreat, bGivenNotThreat)

          val Threat = Flip(pThreat)
          val Ind = If(Threat, betaGivenThreat, betaGivenNotThreat)
          val Alert = Flip(Ind)

          val inferenceTarget = Alert
          val inferenceTargetValue = true
          val alg = VariableElimination(inferenceTarget)
          alg.start()
          val prob = alg.probability(inferenceTarget, inferenceTargetValue)
          println(s"Prob: $prob")
          alg.kill()
          prob
        }
        (probs.last - probs.head) / (2.0 * delta)
      }
      val expectedDerivative = computeDerivative
      println(s"Expected derivative: $expectedDerivative")

      Universe.createNew() // it's critical that we clear the universe after computing expected derivative
      // this is hacky. needs an edit to baseline figaro
      com.cra.figaro.util.setSeed(4444545)
      val pThreat = Constant(0.25)
      val aGivenThreat = Constant(4.0)
      val bGivenThreat = Constant(9.0)
      val aGivenNotThreat = Constant(6.0)
      val bGivenNotThreat = Constant(12.0)
      val betaGivenThreat = Beta(aGivenThreat, bGivenThreat)
      val betaGivenNotThreat = Beta(aGivenNotThreat, bGivenNotThreat)

      val Threat = Flip(pThreat)
      val Ind = If(Threat, betaGivenThreat, betaGivenNotThreat)
      val Alert = Flip(Ind)

      val (prob, deriv) = runADVEandVE(pThreat, Alert, true)
      checkValue(deriv, expectedDerivative)
    }

    // this test should work
    "work on Jeff's Model2Indicator" in {
      val pThreat = 0.25
      val pInd1GivenThreat = Constant(0.7)
      val pInd1GivenNotThreat = Constant(0.15)
      val pInd2GivenThreat = Constant(0.7)
      val pInd2GivenNotThreat = Constant(0.15)
      val pAlertGivenInd1AndInd2 = Constant(0.9)
      val pAlertGivenInd1AndNotInd2 = Constant(0.72)
      val pAlertGivenNotInd1AndInd2 = Constant(0.64)
      val pAlertGivenNotInd1AndNotInd2 = Constant(0.18)

      val Threat = Flip(pThreat)
      val Ind1 = Flip(If(Threat, pInd1GivenThreat, pInd1GivenNotThreat))
      val Ind2 = Flip(If(Threat, pInd2GivenThreat, pInd2GivenNotThreat))

      val Alert = CPD(Ind1, Ind2,
        (false, false) -> Flip(pAlertGivenNotInd1AndNotInd2),
        (false, true) -> Flip(pAlertGivenNotInd1AndInd2),
        (true, false) -> Flip(pAlertGivenInd1AndNotInd2),
        (true, true) -> Flip(pAlertGivenInd1AndInd2))

      val expectedDerivative = 0.139375
      val (prob, deriv) = runADVEandVE(pAlertGivenInd1AndInd2, Alert, true)
      checkValue(deriv, expectedDerivative)
    }

    // this one gets the wrong answer, haven't computed the right one yet.
    "work on Jeff's Model3IndicatorDownSelect" in {
      val pThreat = 0.25
      val pInd1GivenThreat = Constant(0.7)
      val pInd1GivenNotThreat = Constant(0.15)
      val pInd2GivenThreat = Constant(0.7)
      val pInd2GivenNotThreat = Constant(0.15)
      val pInd3GivenThreat = Constant(0.7)
      val pInd3GivenNotThreat = Constant(0.15)

      val pAlertGivenInd1AndInd2AndInd3 = Constant(0.9)
      val pAlertGivenNotInd1Ind2Ind3 = Constant(0.18)
      val pAlertGivenInd1NotInd2Ind3 = Constant(0.72)
      val pAlertGivenInd1Ind2NotInd3 = Constant(0.64)
      val pAlertGivenNotInd1NotInd2Ind3 = Constant(0.9)
      val pAlertGivenNotInd1Ind2NotInd3 = Constant(0.72)
      val pAlertGivenInd1NotInd2NotInd3 = Constant(0.64)
      val pAlertGivenNotInd1NotInd2NotInd3 = Constant(0.18)

      val Threat = Flip(pThreat)
      val Ind1 = Flip(If(Threat, pInd1GivenThreat, pInd1GivenNotThreat))
      val Ind2 = Flip(If(Threat, pInd2GivenThreat, pInd2GivenNotThreat))
      val Ind3 = Flip(If(Threat, pInd3GivenThreat, pInd3GivenNotThreat))

      val Alert = CPD(Ind1, Ind2, Ind3,
        (true, true, true) -> Flip(pAlertGivenInd1AndInd2AndInd3),
        (false, true, true) -> Flip(pAlertGivenNotInd1Ind2Ind3),
        (true, false, true) -> Flip(pAlertGivenInd1NotInd2Ind3),
        (true, true, false) -> Flip(pAlertGivenInd1Ind2NotInd3),
        (false, false, true) -> Flip(pAlertGivenNotInd1NotInd2Ind3),
        (false, true, false) -> Flip(pAlertGivenNotInd1Ind2NotInd3),
        (true, false, false) -> Flip(pAlertGivenInd1NotInd2NotInd3),
        (false, false, false) -> Flip(pAlertGivenNotInd1NotInd2NotInd3))

      val expectedDerivative = 0.25
      val (prob, deriv) = runADVEandVE(pAlertGivenInd1AndInd2AndInd3, Alert, true)
      checkValue(deriv, expectedDerivative)
    }
  }

  /**
    * A test function that runs regular VE and AD VE, compares the results, and returns them
    *
    * @param derivativeTarget     the variable to differentiate
    * @param inferenceTarget      the variable to marginalize to
    * @param inferenceTargetValue the value to check
    * @return a tuple of probability of inferenceTargetValue and the derivative of derivativeTarget at that value
    */
  def runADVEandVE(derivativeTarget: Constant[Double], inferenceTarget: Element[Boolean], inferenceTargetValue: Boolean) = {
    println("First running vanilla VE to find the right probability")
    val baselineAlg = VariableElimination(inferenceTarget)
    baselineAlg.start()
    val baselineProb = baselineAlg.probability(inferenceTarget, inferenceTargetValue)
    baselineAlg.kill()

    println("Now running AD VE")
    val alg = ADVariableElimination.debugged(derivativeTarget, inferenceTarget)
    alg.start()
    val (prob, deriv) = alg.getProbabilityAndDerivative(inferenceTarget, inferenceTargetValue)
    alg.kill()

    println("Comparing results to check probability")
    checkValue(baselineProb, prob)

    println(s"Returning prob: $prob and deriv: $deriv")
    (prob, deriv)
  }

  def checkValue(result: Double, ideal: Double) = {
    val tolerance = 1e-9
    result should be (ideal +- tolerance)
  }
}

