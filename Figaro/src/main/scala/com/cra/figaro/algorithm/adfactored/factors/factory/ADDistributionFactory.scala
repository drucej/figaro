/*
 * DistributionFactory.scala
 * Methods to create factors for simple distributions.
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
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.algorithm.structured.ComponentCollection
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.library.compound.If

/**
  * Modifications from regular DistributionFactory:
  *  - didn't include all methods
  *  - didn't implement full capability
  *  - changed output to Factor[Double, Double]
  *  - changed Factor creation to use Dual Semiring
  *  - added checks to check which Elements should get the dual bit
  **/
object ADDistributionFactory {

  /**
    * Factor constructor for an AtomicFlip
    */
  def makeFactors(cc: ComponentCollection, flip: AtomicFlip): List[Factor[(Double, Double)]] = {
    val flipVar = Factory.getVariable(cc, flip)
    if (flipVar.range.exists(!_.isRegular)) {
      //@TODO handle star
      throw new IllegalStateException("Can't handle Star in ADDistributionFactory")
    } else {
      val factor = new DenseFactor[(Double, Double)](List(), List(flipVar), SumProductDualSemiring())
      val i = flipVar.range.indexOf(Regular(true))
      // DUAL_BIT
      val isDual = ADFactory.isDerivativeTarget(flip)
      val dualProb = (flip.prob, if (isDual) 1.0 else 0.0)
      val dualNotProb = factor.semiring.sum(factor.semiring.one, factor.semiring.product((-1.0, 0.0), dualProb))
      factor.set(List(i), dualProb)
      factor.set(List(1 - i), dualNotProb)
      List(factor)
    }
  }

  /**
    * Factor constructor for a CompoundFlip
    */
  def makeFactors(cc: ComponentCollection, flip: CompoundFlip): List[Factor[(Double, Double)]] = {
    val flipVar = Factory.getVariable(cc, flip)
    val probVar = Factory.getVariable(cc, flip.prob)
    makeCompoundFlip(flipVar, probVar)
  }

  private def makeCompoundFlip(flipVar: Variable[Boolean], probVar: Variable[Double]): List[Factor[(Double, Double)]] = {
    val factor = new DenseFactor[(Double, Double)](List(probVar), List(flipVar), SumProductDualSemiring())
    val parentVals = probVar.range
    if (flipVar.range.exists(!_.isRegular)) {
      // TODO handle star
      throw new IllegalStateException("Can't handle Star in ADDistributionFactory")
    } else {
      val trueIndex = flipVar.range.indexOf(Regular(true))
      val falseIndex = 1 - trueIndex

      def doStuff(row: Int, isDual: Boolean) = {
        val value = (parentVals(row).value, if (isDual) 1.0 else 0.0)
        val dualNotValue = factor.semiring.sum(factor.semiring.one, factor.semiring.product((-1.0, 0.0), value))
        factor.set(List(row, trueIndex), value)
        factor.set(List(row, falseIndex), dualNotValue)
      }

      probVar match {
        case ev: ElementVariable[_] =>
          ev.element match {
            case c: Constant[_]  =>
              // DUAL_BIT
              assert(parentVals.length == 1)
              doStuff(0, ADFactory.isDerivativeTarget(c))
            case i: If[_] =>
              // TODO check assumption that parent values are ordered {thn, else}!
              // DUAL_BIT
              if (parentVals.length == 2) {
                doStuff(0, ADFactory.isDerivativeTarget(i.thn))
                doStuff(1, ADFactory.isDerivativeTarget(i.els))
              } else {
                for {j <- 0 until parentVals.size} {
                  val isDual = ADFactory.isDerivativeTarget(i.thn) || ADFactory.isDerivativeTarget(i.els) || ADFactory.isDerivativeTarget(i)
                  val value = (parentVals(j).value, if (isDual) 1.0 else 0.0)
                  val dualNotValue = factor.semiring.sum(factor.semiring.one, factor.semiring.product((-1.0, 0.0), value))
                  factor.set(List(j, trueIndex), value)
                  factor.set(List(j, falseIndex), dualNotValue)
                }
              }
//              assert(parentVals.length == 2)
            case _ =>
              throw new IllegalStateException("Can't handle this, not a supported Element: " + ev.element)
          }
        case _ =>
          throw new IllegalStateException("Can't handle this, not an ElementVariable: " + probVar)
      }
      List(factor)
    }
  }

}

