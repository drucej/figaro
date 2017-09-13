/*
 * SelectFactory.scala
 * Methods to create factors for Select and Dist elements.
 *
 * Created By:      Glenn Takata (gtakata@cra.com)
 * Creation Date:   Dec 15, 2014
 *
 * Copyright 2014 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.factored.factors.factory.Factory
import com.cra.figaro.algorithm.lazyfactored.{Star, _}
import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.util._

/**
  * Modifications from regular SelectFactory:
  *  - didn't include all methods
  *  - didn't implement full capability
  *  - changed output to Factor[Double, Double]
  *  - changed Factor creation to use Dual Semiring
  *  - added checks to check which Elements should get the dual bit
  **/
object ADSelectFactory {

  /**
    * Factor constructor for an AtomicSelect
    */
  def makeFactors[T](cc: ComponentCollection, select: AtomicSelect[T]): List[Factor[(Double, Double)]] = {
    val selectVar = Factory.getVariable(cc, select)
    if (selectVar.range.exists(!_.isRegular)) {
      throw new IllegalStateException("Cannot handle star")
    } else {
      val probs = getProbs(cc, select)
      List(makeSimpleDistribution(selectVar, probs))
    }
  }

  private def getProbs[U, T](cc: ComponentCollection, select: Select[U, T]): List[U] = getProbs(cc, select, select.clauses)

  /**
    * Get the potential (probability) for each value of an element, based on supplied rules
    */
  def getProbs[U, T](cc: ComponentCollection, elem: Element[T], clauses: List[(U, T)]): List[U] = {
    val selectVar = Factory.getVariable(cc, elem)

    def getProb(xvalue: Extended[T]): U = {
      clauses.find(_._2 == xvalue.value).get._1 // * cannot be a value of a Select
    }

    val probs =
      for {xvalue <- selectVar.range.filter(_.isRegular)} yield getProb(xvalue)
    probs
  }

  /**
    * Constructs a DenseFactor from a probability distribution. It assumes that the probabilities
    * are assigned to the Variable in the same order as it's values.
    */
  def makeSimpleDistribution[T](target: Variable[T], probs: List[Double]): Factor[(Double, Double)] = {
    val factor = new DenseFactor[(Double, Double)](List(), List(target), SumProductDualSemiring())
    for {(prob, index) <- probs.zipWithIndex} {
      val dualBit = if(ADFactory.isDerivativeTarget(target)) 1.0 else 0.0
      factor.set(List(index), (prob, dualBit))
    }
    factor
  }
}
