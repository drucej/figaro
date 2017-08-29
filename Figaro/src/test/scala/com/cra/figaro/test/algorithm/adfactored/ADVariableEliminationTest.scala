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
import com.cra.figaro.library.compound.If
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

    "show Jeff how to create test cases by replicating this block" in {
      val pThreat =  Constant(0.25)
      val pIndGivenThreat = Constant(0.9)
      val pIndGivenNotThreat = Constant(0.15)
      val threat = Flip(pThreat)
      val indicator =  Flip(If(threat,pIndGivenThreat,pIndGivenNotThreat))

      val expectedDerivative = 0.25
      val (prob, deriv) = runADVEandVE(pIndGivenThreat, indicator, true)
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

