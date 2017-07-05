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
import scala.collection.mutable.{ Map, Set }
import scala.language.existentials
import com.cra.figaro.algorithm.factored.factors.factory.Factory


trait AutomaticDifferentiationVariableEliminationDan extends VariableElimination[(Double, Double)] {

  def getDualNodes : List[Element[_]]
  /**
    * AD uses a sum-product-dual semiring.
    */
  override val semiring = SumProductDualSemiring()



  def getFactors(neededElements: List[Element[_]], targetElements: List[Element[_]], upper: Boolean = false): List[Factor[(Double, Double)]] = {
    if (debug) {
      println("Elements (other than thisUniverseFactorsExceptDuals) appearing in factors and their ranges:")
      for { element <- neededElements } {
        println(Variable(element).id + "(" + element.name.string + "@" + element.hashCode + ")" + ": " + element + ": " + Variable(element).range.mkString(","))
      }
    }

    // Before you pass in factors to VE, get rid of intermediate variable - shouldn't have to be in dual space

    // Go through elements, pull out all chains
    // needed elements, split into chain not chain
    // not chain is the same
    // for chain, get list of N+1 factors (n possible outcomes for parents)
    /// for set of factors, multiply together (get big factr)
    // sum out intermediate variable - not an element variable -> is the intermediate variable
    // should be left with N+1 variables in 1 factor
    println("neededElements:" + neededElements) 

    val chains = neededElements.filter { p => p.isInstanceOf[Chain[_,_]] } // all elements that implement chain
//    val nonChains = neededElements.filter { p => !p.isInstanceOf[Chain[_,_]] } // all elements that do not implement chains
    val nonChains = neededElements.diff(chains)
    
    val chainVar = chains.map(Variable(_))

    val chainsFactors = chains flatMap (Factory.makeFactorsForElement(_))
    val nonChainsFactors = nonChains flatMap (Factory.makeFactorsForElement(_))
    
    val productOfFactors = chainsFactors:::nonChainsFactors reduceLeft (_.product(_))
    val factorsInProduct = List(productOfFactors)
    println("factorsInProduct are : " + factorsInProduct)
    
    println("productOfFactors are : " + productOfFactors)
//    val marginalzedFactors = productOfFactors.sumOver(chainVar)
//    println("MF are: " + marginalzedFactors)

/*
 *  val thisUniverseFactorsExceptDual = nonChains.filterNot(p => getDualNodes.contains(p)) flatMap (Factory.makeFactorsForElement(_))
 *  
 */
    
//    val thisUniverseFactorsExceptDual = neededElements.filterNot(p => getDualNodes.contains(p)) flatMap (Factory.makeFactorsForElement(_))
    val thisUniverseFactorsExceptDual = factorsInProduct.filterNot(p => getDualNodes.contains(p)) 

    println("thisUniverseFactorsExceptDual :" + thisUniverseFactorsExceptDual)
    
    val dualFactors = getDualNodes flatMap (Factory.makeFactorsForElement(_))
    println("getDualNodes :" + getDualNodes)

    val variablesForDuals = getDualNodes.map(Variable(_))
    val (factorsWithDuals,factorsWithoutDuals) = thisUniverseFactorsExceptDual.partition(x => x.variables.intersect(variablesForDuals).nonEmpty)
    println("factorsWithDuals are : " + factorsWithDuals)
    println("factorsWithoutDuals are : " + factorsWithoutDuals)

    val dependentUniverseFactors =
      for { (dependentUniverse, evidence) <- dependentUniverses } yield Factory.makeDependentFactor(Variable.cc, universe, dependentUniverse, dependentAlgorithm(dependentUniverse, evidence))

      
    val thisUniverseFactorsExceptDual_conv = factorsWithoutDuals.map(s => convert(s, false))
    val dualFactors_conv = dualFactors.map(s => convert(s, true)):::factorsWithDuals.map(s => convert(s, true))
    val dependentUniverseFactors_conv = dependentUniverseFactors.map(s => convert(s, false))

    val ret = dependentUniverseFactors_conv ::: thisUniverseFactorsExceptDual_conv ::: dualFactors_conv
    
    ret
//        dependentUniverseFactors_conv ::: thisUniverseFactorsExceptDual_conv ::: thisUniverseFactors_conv
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

class ProbQueryVariableEliminationAutoDiff[T](override val universe: Universe, dualNodes: List[Element[_]], target: Element[_])(
  val showTiming: Boolean,
  val dependentUniverses: List[(Universe, List[NamedEvidence[_]])],
  val dependentAlgorithm: (Universe, List[NamedEvidence[_]]) => () => Double)
  extends OneTimeProbQuery
    with AutomaticDifferentiationVariableEliminationDan
{
  lazy val queryTargets = List(target)
  def getDualNodes = dualNodes
  /**
    *  The variable elimination eliminates all variables except on all decision nodes and their parents.
    *  Thus the target elements is both the decision element and the parent element.
    */
  val targetElements = List(target)

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

object AutoDiffVariableElimination {

  def apply[T](adNodes: List[Element[_]], target: Element[T])(implicit universe: Universe) = {
    new ProbQueryVariableEliminationAutoDiff[T](universe, adNodes, target)(
      false,
      List(),
      (u: Universe, e: List[NamedEvidence[_]]) => () => ProbEvidenceSampler.computeProbEvidence(10000, e)(u))
  }

}
