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

import com.cra.figaro.algorithm.factored.{ADVariableElimination, VariableElimination}
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{CPD, If}
import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class ADVariableEliminationTest extends WordSpec with Matchers {

  "ADVE" should {
    "get the same answers as Dan's example" in {
      val pThreat =  Constant(0.25)
      val pIndGivenThreat = Constant(0.9)
      val pIndGivenNotThreat = Constant(0.15)
      val threat = Flip(pThreat)
      val indicator =  Flip(If(threat,pIndGivenThreat,pIndGivenNotThreat))

      val expectedDerivative = 0.25
      val (prob, deriv) = runADVEandVE(pIndGivenThreat, indicator, true)
      checkValue(deriv, expectedDerivative)
    }

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

      val expectedDerivative = 0.25
      val (prob, deriv) = runADVEandVE(pAlertGivenInd1AndInd2, Alert, true)
      checkValue(deriv, expectedDerivative)
    }

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

