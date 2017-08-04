/*
 * StructuredProbQueryAlgorithm.scala
 * SFI algorithms that compute conditional probabilities of queries.
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   December 30, 2015
 *
 * Copyright 2015 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.language._
import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.factors.{Factor, SumProductSemiring, SumProductDualSemiring}
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.lazyfactored.Extended
import com.cra.figaro.algorithm.structured.solver.Solution

abstract class StructuredProbQueryAlgorithm(val universe: Universe, val queryTargets: Element[_]*)
    extends StructuredAlgorithm with DualProbQueryAlgorithm  {

  override def problemTargets = queryTargets.toList

  // Solutions are factors marginalized to individual targets.
  protected var targetFactors: Map[Element[_], Factor[(Double,Double)]] = Map()

  override def processSolutions(solutions: Map[Bounds, Solution]): Unit = {
    if(solutions.size > 1) {
      throw new IllegalArgumentException("this model requires lower and upper bounds; " +
        "use a lazy algorithm instead, or a ranging strategy that avoids *")
    }
    val (solution, _) = solutions.head._2
    val joint = solution.foldLeft(Factory.unit(SumProductDualSemiring()))(_.product(_))
    val m = queryTargets.map( { target => (target, marginalizedTargetFactor(target, joint))})
    m.toMap[Element[_], Factor[(Double,Double)]]   // targetFactors = queryTargets.map(target => (target, marginalizedTargetFactor(target, joint))).toMap
  }

  /**
   * Compute the (normalized) marginalization of the joint factor to the given target element.
   */
  protected def marginalizedTargetFactor[T](target: Element[T], jointFactor: Factor[(Double,Double)]): Factor[(Double,Double)] = {
    val targetVar = collection(target).variable
    val unnormalizedTargetFactor = jointFactor.marginalizeTo(targetVar)
    val z = unnormalizedTargetFactor.foldLeft(SumProductDualSemiring().zero, SumProductDualSemiring().sum(_, _))
    unnormalizedTargetFactor.mapTo((d: (Double,Double)) => SumProductDualSemiring().divide(d, z))
  }

  /**
   * Computes the normalized distribution over a single target element.
   * Throws an IllegalArgumentException if the range of the target contains star.
   */
  def computeDistribution[T](target: Element[T]): Stream[((Double,Double), T)] = {
    val targetVar = collection(target).variable
    if (targetVar.valueSet.hasStar) {
      throw new IllegalArgumentException("target range contains *; " +
        "use a lazy algorithm instead, or a ranging strategy that avoids *")
    }
    val factor = targetFactors(target)
    val dist = factor.getIndices.map(indices => (factor.get(indices), targetVar.range(indices.head).value))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

  /**
   * Computes the expectation of a given function for single target element.
   * Throws an IllegalArgumentException if the range of the target contains star.
   */
  def computeExpectation[T](target: Element[T], function: T => (Double,Double)): (Double,Double) = {
    def get(pair: ((Double,Double), T)) = SumProductDualSemiring().product(pair._1 , function(pair._2))
    val sd = SumProductDualSemiring()
    val e  = (SumProductDualSemiring().zero /: computeDistribution(target))((v1: (Double, Double), v2: ((Double, Double), T)) => sd.sum(v1, v2._1))
    e

  }

  def distribution(target: List[Element[_]]): (List[(String, ProblemComponent[_])], List[((Double,Double), List[Extended[_]])]) = {
    val targetVars = target.map(collection(_).variable)
    val jointFactor = problem.solution.foldLeft(Factory.unit(SumProductDualSemiring()))(_.product(_))
    val unnormalizedTargetFactor = jointFactor.marginalizeTo(targetVars: _*)
    val z = unnormalizedTargetFactor.foldLeft(SumProductDualSemiring().zero, SumProductDualSemiring().sum(_, _))
    val targetFactor = unnormalizedTargetFactor.mapTo((d: (Double,Double)) => SumProductDualSemiring().divide(d, z))
    val components = nameComponents(target, targetFactor)
    val dist = targetFactor.getIndices.map(f => (targetFactor.get(f), targetFactor.convertIndicesToValues(f))).toList
    (components, dist)
  }
  
  private def nameComponents(targets: Seq[Element[_]], factor: Factor[_]): List[(String, ProblemComponent[_])] = {
    val targetVars: Seq[(String, ProblemComponent[_])] = targets.map(t => (t.name.string, collection(t)))
    val variables = factor.variables
    val mappedElementNames = targetVars.map(t => (t._1, t._2, variables.indexOf(t._2.variable))).sortBy(_._3).toList
    for ((name, component, pos) <- mappedElementNames) yield (name, component)
  }

}

trait OneTimeStructuredProbQuery extends StructuredProbQueryAlgorithm with OneTimeStructured with OneTimeDualProbQuery

trait AnytimeStructuredProbQuery extends StructuredProbQueryAlgorithm with AnytimeStructured with AnytimeProbQueryDual
