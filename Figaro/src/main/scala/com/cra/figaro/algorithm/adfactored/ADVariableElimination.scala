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

package com.cra.figaro.algorithm.factored

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.util._

import scala.language.postfixOps
import com.cra.figaro.algorithm.factored.factors.factory.{ADFactory, Factory}

/**
  * Modifications from regular VE
  *  - changed apply() call to create the algorithm
  *  - register w/ Factory to store the derivative target
  *  - added a call to check derivative
  *  - changed output to Factor[Double, Double]
  *  - changed math to use Dual Semiring
  *  - didn't implement full capability (some legacy methods will have unpredictable results)
  */

/**
 * Trait of algorithms that perform variable elimination.
 *
 */
trait ADVariableElimination[T] extends VariableElimination[(T, Double)] {

}

/**
 * Variable elimination over probabilistic factors.
 */
trait ADProbabilisticVariableElimination extends ADVariableElimination[Double] {
  def getFactors(allElements: List[Element[_]], targetElements: List[Element[_]], upper: Boolean = false): List[Factor[(Double, Double)]] = {
    if (debug) {
      println("Elements appearing in factors and their ranges:")
      for {element <- allElements} {
        println(Variable(element).id + "(" + element.name.string + "@" + element.hashCode + ")" + ": " + element + ": " + Variable(element).range.mkString(","))
      }
    }
    val thisUniverseFactors = allElements flatMap (ADFactory.makeFactorsForElement(_))
    val dependentUniverseFactors =
      for {(dependentUniverse, evidence) <- dependentUniverses} yield ADFactory.makeDependentFactor(Variable.cc, universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))
    dependentUniverseFactors ::: thisUniverseFactors
  }
}

/**
 * Variable elimination algorithm that computes the conditional probability of query elements.
 *
 */
class ADProbQueryVariableElimination(override val universe: Universe, adTarget: Element[_], targets: Element[_]*)(
  val showTiming: Boolean,
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends OneTimeProbQuery
  with ADProbabilisticVariableElimination {
  val targetElements = targets.toList
  lazy val queryTargets = targets.toList

  ADFactory.derivativeTarget = adTarget

  val semiring = SumProductDualSemiring()

  private def marginalizeToTarget(factor: Factor[(Double,Double)], target: Element[_]): Unit = {
    val unnormalizedTargetFactor = factor.marginalizeTo(Variable(target))
    val z = unnormalizedTargetFactor.foldLeft(semiring.zero, (x: (Double, Double), y: (Double, Double)) => semiring.sum(x,y))
    println("normalization term is :" + z)
    println("Unnormalized factor is :" + unnormalizedTargetFactor)
    val targetFactor = unnormalizedTargetFactor.mapTo((d: (Double, Double)) => semiring.divide(d,z))
    println("normalized factor is :" + targetFactor)
    targetFactors += target -> targetFactor
  }

  private def marginalize(resultFactor: Factor[(Double, Double)]) =
    targets foreach (marginalizeToTarget(resultFactor, _))

  private def makeResultFactor(factorsAfterElimination: MultiSet[Factor[(Double, Double)]]): Factor[(Double, Double)] = {
    // It is possible that there are no factors (this will happen if there are  no queries or evidence).
    // Therefore, we start with the unit factor and use foldLeft, instead of simply reducing the factorsAfterElimination.
    factorsAfterElimination.foldLeft(Factory.unit(semiring))(_.product(_))
  }

  def finish(factorsAfterElimination: MultiSet[Factor[(Double,Double)]], eliminationOrder: List[Variable[_]]) =
    marginalize(makeResultFactor(factorsAfterElimination))

  /**
   * Computes the normalized distribution over a single target element.
   */
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val distDual = computeDistributionDual(target)
    val str = distDual.map {
      case ((aProb: Double, aDual: Double), aValue) =>
        (aProb, aValue)
    }
    str
  }

  /**
    * Computes the normalized distribution over a single target element.
    */
  def computeDistributionDual[T](target: Element[T]): Stream[((Double, Double), T)] = {
    val factor = targetFactors(target)
    if (factor.numVars > 1) throw new UnsupportedAlgorithmException(target)
    val targetVar = if (factor.output.nonEmpty) factor.output.head.asInstanceOf[Variable[T]] else factor.parents.head.asInstanceOf[Variable[T]]
    val dist: Iterable[((Double, Double), T)] = factor.getIndices.filter(f => targetVar.range(f.head).isRegular).map(f => (factor.get(f), targetVar.range(f.head).value))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

  /**
    * Computes the expectation of a given function for single target element.
    */
  def computeExpectationDual[T](target: Element[T], function: T => Double): (Double, Double) = {
    def doFunction(pair: ((Double, Double), T)) = semiring.product(pair._1, function(pair._2))
    val distrDual = computeDistributionDual(target)
    val exp = distrDual.foldLeft(semiring.zero)({ (v1: (Double, Double), v2: ((Double, Double), T)) => {
      val functionResult: (Double, Double) = doFunction(v2)
      semiring.sum(v1, functionResult)
    } })
    exp
  }

  /**
    * Computes the expectation of a given function for single target element.
    */
  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    val exp = computeExpectationDual(target, function)
    exp._1
  }

  /**
    * Gets the probability that some variable equals a target value _and_
    * the derivative of the derivative target at that point.
    * The derivative target is set during construction.
    *
    * TODO this is a little ugly. not protecting user from, other, inherited versions of probabiltity() that won't work
    * @param target - inference target, NOT derivative target
    * @param value - test value
    * @tparam T
    * @return
    */
  def getProbabilityAndDerivative[T](target: Element[T], value: T): (Double, Double) = {
    check(target)
    val predicate = (t: T) => t == value
    val exp = computeExpectationDual(target, (t: T) => if (predicate(t)) 1.0; else 0.0)
    exp
  }
}

object ADVariableElimination {
  /**
    * Create a variable elimination computer with the given target query variables in the current default
    * universe.
    */
  def apply(adTarget: Element[_], targets: Element[_]*)(implicit universe: Universe) =
    new ADProbQueryVariableElimination(universe, adTarget, targets: _*)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

  /**
    * Create a variable elimination computer with the given target query variables in the current default
    * universe, with debug information enabled.
    */
  def debugged(adTarget: Element[_], targets: Element[_]*)(implicit universe: Universe) =
    new ADProbQueryVariableElimination(universe, adTarget, targets: _*)(
      true,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u)) { debug = true }

  /**
    * Create a variable elimination computer with the given target query variables in the current default
    * universe, with timing information enabled.
    */
  def timed(adTarget: Element[_], targets: Element[_]*)(implicit universe: Universe) =
    new ADProbQueryVariableElimination(universe, adTarget, targets: _*)(
      true,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))

}


