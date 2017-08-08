/*
 * AutoDiffVariableElimination.scala
 * Variable elimination equipped with AD to provide gradients.
 *
 * Created By:      Brian Ruttenberg (bruttenberg@cra.com)
 * Creation Date:   Oct 1, 2012
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.AutomaticDifferentiation

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.decision._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.lazyfactored.Extended

import annotation.tailrec
import scala.collection.mutable.{Map, Set}
import scala.language.existentials
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.structured.ChainComponent


trait AutomaticDifferentiationVariableEliminationDan extends VariableElimination[(Double, Double)] {

  def getDualNodes : List[Element[_]]
  /**
    * AD uses a sum-product-dual semiring.
    */
  override val semiring = SumProductDualSemiring()


  def recusivelyAccumulateArguments(target: Element[_], accum: List[Element[_]]): List[Element[_]] = {
    if (target.args.isEmpty)
      {
        return target :: accum
      }
    return target :: target.args.foldLeft(accum)(_ ::: recusivelyAccumulateArguments  (_, accum))
  }

  def getDependencies(target: Element[_]): List[Element[_]] = {
    return recusivelyAccumulateArguments(target, List.empty[Element[_]])
  }
  def elementIsRelatedToTarget(e: Element[_], targetElement: Element[_]): Boolean = {
    val suspectRange = Variable(e).range
    val targetRange = Variable(targetElement).range
    return suspectRange.intersect(targetRange).size > 0
  }

  def getFactors(neededElements: List[Element[_]], targetElements: List[Element[_]], upper: Boolean = false): List[Factor[(Double, Double)]] = {
    if (debug) {
      println("Elements (other than thisUniverseFactorsExceptDuals) appearing in factors and their ranges:")
      for { element <- neededElements } {
        println(Variable(element).id + "(" + element.name.string + "@" + element.hashCode + ")" + ": " + element + ": " + Variable(element).range.mkString(","))
      }
    }
    val allFactors = neededElements flatMap (Factory.makeFactorsForElement(_))
    allFactors
  }

  private def convert(f: Factor[Double], isDual: Boolean): Factor[(Double, Double)] = {

    if (!isDual) {
      f.mapTo[(Double, Double)]((d: Double) => (d, 0.0), semiring.asInstanceOf[Semiring[(Double, Double)]])
    } else {
      //      if (f.variables.length > 1 && f.variables.head.range.size > 1) throw new IllegalUtilityNodeException
      val newF = f.mapTo[(Double, Double)]((d: Double) => (d, 1.0 ), semiring.asInstanceOf[Semiring[(Double, Double)]])
      newF
    }

  }

}

class ProbQueryVariableEliminationAutoDiff[T](override val universe: Universe, derivativeTargets: List[Element[_]], targetQuery: Element[_])(
  val showTiming: Boolean,
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends OneTimeDualProbQuery
    with AutomaticDifferentiationVariableEliminationDan
{
  lazy val queryTargets = List(targetQuery)
  def getDualNodes = derivativeTargets
  /**
    *  The variable elimination eliminates all variables except on all decision nodes and their parents.
    *  Thus the target elements is both the decision element and the parent element.
    */
  val targetElements = List(targetQuery)

  private var finalFactors: Factor[(Double, Double)] = Factory.defaultFactor[(Double, Double)](List(), List(), semiring)

  /* Marginalizes the final factor using the semiring for decisions
   *
   */
  private def marginalizeToTarget(factor: Factor[(Double, Double)], target: Element[_]): Unit = {
    val unnormalizedTargetFactor = factor.marginalizeTo(Variable(target))
    val z = unnormalizedTargetFactor.foldLeft(semiring.zero, (x: (Double, Double), y: (Double, Double)) => semiring.sum(x,y))
    println("normalization term is :" + z)
    println("Unnormalized factor is :" + unnormalizedTargetFactor)
    val targetFactor = unnormalizedTargetFactor.mapTo((d: (Double, Double)) => semiring.divide(d,z))
    println("normalized factor is :" + targetFactor)
    targetFactors += target -> targetFactor
  }

  private def marginalize(resultFactor: Factor[(Double, Double)]) =
    queryTargets foreach (marginalizeToTarget(resultFactor, _))

  private def makeResultFactor(factorsAfterElimination: MultiSet[Factor[(Double, Double)]]): Factor[(Double, Double)] = {
    factorsAfterElimination.foldLeft(Factory.unit(semiring))(_.product(_))
  }

  def finish(factorsAfterElimination: MultiSet[Factor[(Double, Double)]], eliminationOrder: List[Variable[_]]) =
    marginalize(makeResultFactor(factorsAfterElimination))


  def computeDistribution[T](target: Element[T]): Stream[((Double,Double), T)] = {
    val factor = targetFactors(target)
    if (factor.numVars > 1) throw new UnsupportedAlgorithmException(target)
    val targetVar = if (factor.output.nonEmpty) factor.output.head.asInstanceOf[Variable[T]] else factor.parents.head.asInstanceOf[Variable[T]]
    val dist = factor.getIndices.filter(f => targetVar.range(f.head).isRegular).map(f => (factor.get(f), targetVar.range(f.head).value))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }
  def computeExpectation[T](target: Element[T], function: T => (Double,Double)): (Double,Double) = {
    def get(pair: ((Double,Double), T)) = semiring.product(pair._1, function(pair._2))
    //    (0.0 /: computeDistribution(target))(_ + get(_))

    (semiring.zero /: computeDistribution(target))( { (v1: (Double, Double), v2: ((Double, Double), T))  => semiring.sum(v1, v2._1)})

  }
}
    /* moved to (double,double) - dscof
  def computeDistribution[T](target: Element[T]): Stream[(Double, T)] = {
    val factor = targetFactors(target)
    val targetVar = Variable(target)
    val dist = targetVar.range.filter(_.isRegular).map(_.value).zipWithIndex map (pair => (factor.get(List(pair._2))._1, pair._1))
    // normalization is unnecessary here because it is done in marginalizeTo
    dist.toStream
  }

  def computeExpectation[T](target: Element[T], function: T => Double): Double = {
    def get(pair: (Double, T)) = pair._1 * function(pair._2)
    (0.0 /: computeDistribution(target))(_ + get(_))
  }
}
*/

object AutoDiffVariableElimination {

  def apply[T](adNodes: List[Element[_]], target: Element[T])(implicit universe: Universe) = {
    new ProbQueryVariableEliminationAutoDiff[T](universe, adNodes, target)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
  }

}
