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

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.algorithm.lazyfactored.Regular
import com.cra.figaro.algorithm.structured.ComponentCollection

/**
 * A Sub-Factory for simple probability distribution Elements
 */
object DistributionFactory {

  /**
   * Factor constructor for an AtomicFlip
   */
  def makeFactors(cc: ComponentCollection, flip: AtomicFlip): List[Factor[(Double, Double)]] = {
    val flipVar = Factory.getVariable(cc, flip)
    if (flipVar.range.exists(!_.isRegular)) {
      assert(flipVar.range.size == 1) // Flip's range must either be {T,F} or {*}
      StarFactory.makeStarFactor(cc, flip)
    } else {
      val factor = new DenseFactor[(Double,Double)](List(), List(flipVar))
      val i = flipVar.range.indexOf(Regular(true))
      val dualProb = (flip.prob, if (flipVar.isDual) 1.0 else 0.0)
      val dualNotProb = factor.semiring.sum(factor.semiring.one, factor.semiring.product((-1.0, 0.0), dualProb))
      factor.set(List(i), dualProb)
      factor.set(List(1 - i), dualNotProb)
      List(factor)
    }
  }

  /**
   * Factor constructor for a CompoundFlip
   */
  def makeFactors(cc: ComponentCollection, flip: CompoundFlip): List[Factor[(Double,Double)]] = {
    val flipVar = Factory.getVariable(cc, flip)
    val probVar = Factory.getVariable(cc, flip.prob)
    makeCompoundFlip(flipVar, probVar)
  }

  private def makeCompoundFlip(flipVar: Variable[Boolean], probVar: Variable[Double]): List[Factor[(Double,Double)]] = {
    println("MakeFactors For Compound Flip: " + flipVar + " probVar " + probVar)
    probVar match {
      case e: ElementVariable[_] => println(e.element)
      case _ => null
    }
    val everUsesDual = !probVar.range.filter(_.isDual).isEmpty
    println("probvar uses dual: " + everUsesDual + " range is: " + probVar.range)
    val factor = new DenseFactor[(Double,Double)](List(probVar), List(flipVar))
    val parentVals = probVar.range
    if (flipVar.range.exists(!_.isRegular)) {
      val falseIndex = flipVar.range.indexOf(Regular(false))
      val trueIndex = flipVar.range.indexOf(Regular(true))
      val starIndex = flipVar.range.indexWhere(!_.isRegular)
      for { j <- 0 until parentVals.size } {
        if (parentVals(j).isRegular) {
          // value is the probability of the parent
          if (parentVals(j).isDual) {
            val value = parentVals(j).value
            val dualValue = (value, 1.0)
            val dualNotValue = factor.semiring.sum(factor.semiring.one, factor.semiring.product((-1.0, 0.0), dualValue))
            factor.set(List(j, trueIndex), dualValue)
            factor.set(List(j, falseIndex), dualNotValue)
            factor.set(List(j, starIndex), factor.semiring.zero)
          }
          else {
            val value = parentVals(j).value
            val dualValue = (value, 0.0)
            val dualNotValue = factor.semiring.sum(factor.semiring.one, factor.semiring.product((-1.0, 0.0), dualValue))
            factor.set(List(j, trueIndex), dualValue)
            factor.set(List(j, falseIndex), dualNotValue)
            factor.set(List(j, starIndex), factor.semiring.zero)

          }
        } else {
          factor.set(List(j, trueIndex), factor.semiring.zero)
          factor.set(List(j, falseIndex), factor.semiring.zero)
          factor.set(List(j, starIndex), factor.semiring.one)
        }
      }
      List(factor)
    } else {
      val trueIndex = flipVar.range.indexOf(Regular(true))
      val falseIndex = 1 - trueIndex
      for { j <- 0 until parentVals.size } {
        val value = (parentVals(j).value, if (parentVals(j).isDual) 1.0 else 0.0)
        val dualNotValue = factor.semiring.sum(factor.semiring.one, factor.semiring.product((-1.0, 0.0), value))
        factor.set(List(j, trueIndex), value)
        factor.set(List(j, falseIndex), dualNotValue)
      }
      List(factor)
    }
  }

  /**
   * Factor constructor for a ParameterizedFlip
   */
  def makeFactors(cc: ComponentCollection, flip: ParameterizedFlip, parameterized: Boolean): List[Factor[(Double,Double)]] = {
    println("MakeFactors For Flip: " + flip)
    if (parameterized) {
      val flipVar = Factory.getVariable(cc, flip)
      val factor = new DenseFactor[(Double,Double)](List(),List(flipVar))
      val prob = flip.parameter.MAPValue
      if (flipVar.range.forall(_.isRegular)) {
        val i = flipVar.range.indexOf(Regular(true))
        if (flipVar.isDual) {
          val dualProb = (prob, 1.0)
          val dualNotProb = factor.semiring.sum(dualProb, factor.semiring.product((-1.0,0), dualProb))
          factor.set(List(i), dualProb)
          factor.set(List(1 - i), dualNotProb)
        } else {
          val dualProb = (prob, 0.0)
          val dualNotProb = factor.semiring.sum(dualProb, factor.semiring.product((-1.0,0), dualProb))
          factor.set(List(i), dualProb)
          factor.set(List(1 - i), dualNotProb)
        }

      } else {
        val trueIndex = flipVar.range.indexOf(Regular(true))
        val falseIndex = flipVar.range.indexOf(Regular(false))
        val starIndex = flipVar.range.indexWhere(!_.isRegular)
        if (flipVar.isDual) {
          val dualProb = (prob, 1.0)
          val dualNotProb = factor.semiring.sum(dualProb, factor.semiring.product((-1.0,0), dualProb))
          factor.set(List(trueIndex), dualProb)
          factor.set(List(falseIndex), dualNotProb)
          factor.set(List(starIndex), factor.semiring.zero)
        }
        else {
          val dualProb = (prob, 1.0)
          val dualNotProb = factor.semiring.sum(dualProb, factor.semiring.product((-1.0,0), dualProb))
          factor.set(List(trueIndex), dualProb)
          factor.set(List(falseIndex), dualNotProb)
          factor.set(List(starIndex), factor.semiring.zero)

        }
      }
      List(factor)
    } else {
      val flipVar = Factory.getVariable(cc, flip)
      val probVar = Factory.getVariable(cc, flip.parameter)
      makeCompoundFlip(flipVar, probVar)
    }
  }

  /**
   * Factor constructor for an AtomicBinomial
   */
  def makeFactors(cc: ComponentCollection, binomial: AtomicBinomial): List[Factor[Double]] = {
      val binVar = Factory.getVariable(cc, binomial)
      val factor = new DenseFactor[Double](List(), List(binVar))
      for { (xvalue, index) <- binVar.range.zipWithIndex } {
        factor.set(List(index), binomial.density(xvalue.value))
      }
    List(factor)
  }

  /**
   * Factor constructor for a parameterized binomial
   */
  def makeFactors(cc: ComponentCollection, binomial: ParameterizedBinomialFixedNumTrials, parameterized: Boolean): List[Factor[(Double, Double)]] = {
    println("parameterized binomial is broken- dscof")
    if (parameterized) {
      val binVar = Factory.getVariable(cc, binomial)
      val factor = new DenseFactor[(Double,Double)](List(),List(binVar))
      if (binVar.range.exists(!_.isRegular)) { // parameter must not have been added since it's an atomic beta
        for { (xvalue, index) <- binVar.range.zipWithIndex } {
          val entry = if (xvalue.isRegular) (0.0, 0.0) else (1.0,0.0)
          factor.set(List(index), entry)
        }
      } else {
        val probSuccess = binomial.parameter.MAPValue
        for { (xvalue, index) <- binVar.range.zipWithIndex } {
          factor.set(List(index), (Util.binomialDensity(binomial.numTrials, probSuccess, xvalue.value), 0.0))
        }
      }
      List(factor)
    } else {
      val binVar = Factory.getVariable(cc, binomial)
      if (binVar.range.exists(!_.isRegular)) { // parameter must not have been added since it's an atomic beta
        val factor = new DenseFactor[(Double,Double)](List(),List(binVar))
        for { (xvalue, index) <- binVar.range.zipWithIndex } {
          val entry = if (xvalue.isRegular) (0.0,0.0) else (1.0,0.0)
          factor.set(List(index), entry)
        }
        List(factor)
      } else {
        ChainFactory.makeFactors(cc, binomial)
      }
    }
  }

}
