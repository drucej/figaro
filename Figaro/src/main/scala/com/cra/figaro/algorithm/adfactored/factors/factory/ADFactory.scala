/*
 * Factory.scala
 * Methods to create factors over variables.
 *
 *
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.factors.factory

import com.cra.figaro.algorithm._
import com.cra.figaro.language._
import com.cra.figaro.util._
import com.cra.figaro.algorithm.lazyfactored._
import com.cra.figaro.algorithm.lazyfactored.ValueSet._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.factored.factors._
import com.cra.figaro.library.compound.{If, _}
import com.cra.figaro.library.collection._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.structured._

import scala.reflect.runtime.universe.{TypeTag, typeTag}
import scala.reflect.runtime.universe.TypeTag


/**
  * Modifications from regular Factory:
  *  - didn't include all methods
  *  - didn't implement full capability
  *  - changed output to Factor[Double, Double]
  *  - changed Factor creation to use Dual Semiring
  *  - added hack to store and check the derivative target variable
  */
object ADFactory {
  /**
    * Create the probabilistic factor encoding the probability of evidence in the dependent universe as a function of the
    * values of variables in the parent universe. The third argument is the the function to use for computing
    * probability of evidence in the dependent universe. It is assumed that the definition of this function will already contain the
    * right evidence.
    */
  def makeDependentFactor(cc: ComponentCollection, parentUniverse: Universe,
                          dependentUniverse: Universe,
                          probEvidenceComputer: () => Double): Factor[(Double, Double)] = {
    val uses = dependentUniverse.parentElements filter (_.universe == parentUniverse)
    def rule(values: List[Any]) = {
      for { (elem, value) <- uses zip values } { elem.value = value.asInstanceOf[Regular[elem.Value]].value }
      val result = probEvidenceComputer()
      (result, 0.0)
    }
    val variables = uses map (cc(_).variable)
    val factor = new DenseFactor[(Double,Double)](variables, List(), SumProductDualSemiring())
    factor.fillByRule(rule _)
    factor
  }

  private def makeAbstract[T](cc: ComponentCollection, elem: Element[T], abstraction: Abstraction[T]): List[Factor[(Double, Double)]] =
    elem match {
     // case apply: Apply1[_, _] => ApplyFactory.makeFactors(cc, apply)(abstraction.scheme)
     // case apply: Apply2[_, _, _] => ApplyFactory.makeFactors(cc, apply)(abstraction.scheme)
     //  case apply: Apply3[_, _, _, _] => ApplyFactory.makeFactors(cc, apply)(abstraction.scheme)
      // In the case of a Chain, its pragmas are inherited by the expanded result elements. The abstraction will be
      // taken into account when we generate factors for the result elements.
      // case chain: Chain[_, _] => ChainFactory.makeFactors(cc, chain)(abstraction.scheme)
      case atomic: Atomic[_] => makeAbstract(cc, atomic, abstraction)
      case _ => throw new UnsupportedAlgorithmException(elem)
    }

  /**
    * Make the non-constraint factors corresponding to the given element in the component collection.
    *
    * @param parameterized If true, parameterized elements are assumed to be equal to their previously computed MAP value. If false, they are treated like any other element.
    */
  def makeFactors[T](cc: ComponentCollection, elem: Element[T], parameterized: Boolean): List[Factor[(Double, Double)]] = {
    val component = cc(elem)
    if (component.range.hasStar && component.range.regularValues.isEmpty) List()
    else {
      Abstraction.fromPragmas(elem.pragmas) match {
        case None => concreteFactors(cc, elem, parameterized)
        case Some(abstraction) => makeAbstract(cc, elem, abstraction)
      }
    }
  }

  /**
    * Copied from Factory - it's private
    */
  private def makeFactors[T](cc: ComponentCollection, const: Constant[T]): List[Factor[Double]] = {
    val factor = new DenseFactor[Double](List(), List(Factory.getVariable(cc, const)))
    factor.set(List(0), 1.0)
    List(factor)
  }


  /**
    * Copied from Factory - it's private
    */
  private def makeFactors[T](cc: ComponentCollection, atomic: Atomic[T]): List[Factor[Double]] = {
    val atomicVar = Factory.getVariable(cc, atomic)
    val atomicComp = cc(atomic)
    // Map each extended value to its probability density, preserving the order of the list
    val probs = atomicVar.range.map(atomicComp.probs)
    List(SelectFactory.makeSimpleDistribution(atomicVar, probs))
  }


  /**
    * Make factors for a particular element. This function wraps the SFI method of creating factors using component collections
    */
  def makeFactorsForElement[Value](elem: Element[_], upper: Boolean = false, parameterized: Boolean = false): Seq[Factor[(Double,Double)]]= {
    val variable = Variable(elem)
    val comp = Variable.cc(elem)
    if (elem.intervention.isDefined) {
      val factor = new DenseFactor[(Double, Double)](List(), List(variable), SumProductDualSemiring())
      factor.set(List(0), (1.0, 0.0))
      List(factor)
    }
    else {
      comp match {
        // If the element is a chain, we need to create subproblems for each value of the chain
        // to create factors accordingly
        case chainComp: ChainComponent[_, _] =>
          val chain = chainComp.chain
          val chainMap = LazyValues(elem.universe).getMap(chain)
          chainMap.foreach(f => {
            val subproblem = new NestedProblem(Variable.cc, f._2)
            Variable.cc.expansions += (chain.chainFunction, f._1) -> subproblem
            chainComp.subproblems = chainComp.subproblems.updated(f._1, subproblem)
          })
        // If the element is a MakeArray, we need mark that it has been expanded. Note that
        // the normal Values call will expand the MakeArray, we are just setting the max expansion here
        case maComp: MakeArrayComponent[_] =>
          val ma = maComp.makeArray
          maComp.maxExpanded = Variable.cc(ma.numItems).range.regularValues.max
        // If the element is an apply, we need to populate the Apply map used by the factor creation
        case applyComp: ApplyComponent[Value] =>
          val apply = applyComp.apply
          val applyMap = LazyValues(elem.universe).getMap(apply)
          applyComp.setMap(applyMap)
        case atomicComp: AtomicComponent[Value] =>
          // The range for this component was generated, but not its distribution
          // This computes the probability mass for each value in the range
          atomicComp.probs = atomicComp.ranger.discretize()
        case _ => ()
      }
      // Make the constraint and non-constraint factors for the element by calling the
      // component factor makers
      val constraint = if (upper) {
        comp.constraintFactors(Upper)
      } else {
        comp.constraintFactors(Lower)
      }
      if (!constraint.isEmpty) {
        throw new IllegalStateException("Not sure what to do with Constraints here in ADFactory")
      }
      //constraint ::: comp.nonConstraintFactors(parameterized)
      makeFactors(comp.problem.collection, comp.element, parameterized).map(_.deDuplicate)
    }
  }

  /**
    * Converts usual factors to AD factors
    * Assumes nothing in the factor is a derivative target
    * @param f
    * @return
    */
  private def convert(f: Factor[Double]): Factor[(Double, Double)] = {
    // DUAL BIT
    val newF = f.mapTo[(Double, Double)]((d: Double) => (d, 0.0 ), SumProductDualSemiring().asInstanceOf[Semiring[(Double, Double)]])
    newF
  }

  /**
    * Invokes Factor constructors for a standard set of Elements. This method uses various
    * secondary factories.
    */
  def concreteFactors[T](cc: ComponentCollection, elem: Element[T], parameterized: Boolean): List[Factor[(Double, Double)]] = {
    // TODO have factor creation for all atomics use the new method above
    elem match {
//      case flip: ParameterizedFlip => DistributionFactory.makeFactors(cc, flip, parameterized)
//      case pSelect: ParameterizedSelect[_] => SelectFactory.makeFactors(cc, pSelect, parameterized)
//      case pBin: ParameterizedBinomialFixedNumTrials => DistributionFactory.makeFactors(cc, pBin, parameterized)
//      case parameter: DoubleParameter if parameterized => makeParameterFactors(cc, parameter)
//      case array: ArrayParameter if parameterized => makeParameterFactors(cc, array)
      case c: Chain[_, _] => ADChainFactory.makeFactors(cc, c)
      case f: AtomicFlip => ADDistributionFactory.makeFactors(cc, f)
      case f: CompoundFlip => ADDistributionFactory.makeFactors(cc, f)
//      case ab: AtomicBinomial => DistributionFactory.makeFactors(cc, ab)
      case s: AtomicSelect[_] => ADSelectFactory.makeFactors(cc, s)
//      case s: CompoundSelect[_] => SelectFactory.makeFactors(cc, s)
//      case d: AtomicDist[_] => SelectFactory.makeFactors(cc, d)
//      case d: CompoundDist[_] => SelectFactory.makeFactors(cc, d)
//      case s: IntSelector => SelectFactory.makeFactors(cc, s)
      case constant: Constant[_] => makeFactors(cc, constant).map(convert(_))
      case a: Apply1[_, _] => ADApplyFactory.makeFactors(cc, a)
      case a: Apply2[_, _, _] => ADApplyFactory.makeFactors(cc, a)
      case a: Apply3[_, _, _, _] => ADApplyFactory.makeFactors(cc, a)
      case a: Apply4[_, _, _, _, _] => ADApplyFactory.makeFactors(cc, a)
      case a: Apply5[_, _, _, _, _, _] => ADApplyFactory.makeFactors(cc, a)
//      case i: Inject[_] => makeFactors(cc, i)
//      case r: SingleValuedReferenceElement[_] => ComplexFactory.makeFactors(cc, r)
//      case r: MultiValuedReferenceElement[_] => ComplexFactory.makeFactors(cc, r)
//      case r: Aggregate[_, _] => ComplexFactory.makeFactors(cc, r)
//      //case m: MakeList[_] => ComplexFactory.makeFactors(cc, m)
//      case m: MakeArray[_] => ComplexFactory.makeFactors(cc, m)
//      case f: FoldLeft[_, _] => ComplexFactory.makeFactors(cc, f)
//      case f: FactorMaker[_] => f.makeFactors
      case a: Atomic[_] => makeFactors(cc, a).map(convert(_))

      case _ => throw new UnsupportedAlgorithmException(elem)
    }
  }
  /*
   * The conditional selector creates a factor in which, when the selector's value is such that the result
   * element is relevant to the final result, the result element and overall element must have the same
   * value (handled by makeCares). Otherwise, the result element and overall element can take on any
   * value (handled by makeDontCares)
   */
  /**
   * Make a conditional selector factor used in the decomposition of chain and other elements.
   * A chain defines a factor over the parent element, each of the possible result elements of the chain,
   * and the overall chain element. This can produce a very large factor when there are many result elements.
   * This is solved by decomposing the chain factor into a product of factors, each of which contains the
   * parent element, one of the result elements, and the overall chain element.
   */
  def makeConditionalSelector[T, U](pairVar: Variable[List[Extended[_]]], parentXVal: Extended[T], outcomeVar: Variable[U], choices: Set[U])(implicit mapper: PointMapper[U]): Factor[(Double, Double)] = {
    val factor = new ConditionalSelector[(Double, Double)](List(pairVar), List(outcomeVar), SumProductDualSemiring())
    for {
      (pairXVal, pairIndex) <- pairVar.range.zipWithIndex
      (outcomeXVal, outcomeIndex) <- outcomeVar.range.zipWithIndex
    } {
      // See makeTupleVarAndFactor: pairXVal is always regular and consists of a list of two elements: the extended parent value
      // and the extended outcome value.
      val List(selectXVal, overallXVal) = pairXVal.value
      val entry =
        if (selectXVal.isRegular && parentXVal.isRegular) {
          if (selectXVal.value == parentXVal.value) {
            if ((!overallXVal.isRegular && !outcomeXVal.isRegular) || (overallXVal.isRegular && outcomeXVal.isRegular && overallXVal.value == mapper.map(outcomeXVal.value, choices))) 1.0 else 0.0
          } else 1.0
        } else if (selectXVal.isRegular || parentXVal.isRegular) 1.0 // they are different
        else if (!overallXVal.isRegular) 1.0 // if parentXVal is *, the only possible outcomeXVal is *
        else 0.0
      factor.set(List(pairIndex, outcomeIndex), (entry, 0.0))
    }
    factor
  }

  /**
   * Given a sequence of variables, create a new variable representing the tuple of the inputs
   * and create the factor mapping the inputs to their tuple.
   * @param inputs the variables to be formed into a tuple
   */
  def makeTupleVarAndFactor(cc: ComponentCollection, chain: Option[Chain[_, _]], inputs: Variable[_]*): (Variable[List[Extended[_]]], Factor[(Double, Double)]) = {
    val inputList: List[Variable[_]] = inputs.toList
    // Subtlety alert: In the tuple, we can't just map inputs with * to *. We need to remember which input was *.
    // Therefore, instead, we make the value a regular value consisting of a list of extended values.
    val tupleRangeRegular: List[List[_]] = cartesianProduct(inputList.map(_.range): _*)
    val tupleVS: ValueSet[List[Extended[_]]] = withoutStar(tupleRangeRegular.map(_.asInstanceOf[List[Extended[_]]]).toSet)
    val tupleVar: Variable[List[Extended[_]]] = if (chain.nonEmpty) Factory.makeVariable(cc, tupleVS, chain.get) else Factory.makeVariable(cc, tupleVS)
    val tupleFactor = new SparseFactor[(Double, Double)](inputList, List(tupleVar), SumProductDualSemiring())
    for { pair <- tupleVar.range.zipWithIndex } {
      val tupleVal: List[Extended[_]] = pair._1.value
      val tupleIndex = pair._2
      val inputIndices =
        for { (input, value) <- inputList.zip(tupleVal) } yield input.range.indexOf(value)
      // DUAL BIT
      tupleFactor.set(inputIndices ::: List(tupleIndex), (1.0, 0.0))
    }
    (tupleVar, tupleFactor)
  }

  // TODO the following methods create state in the ADFactory, which is a hack that breaks the design a bit
  var derivativeTarget: Element[_] = _

  /**
    * NotThreadSafe
    * @param element
    * @return
    */
  def isDerivativeTarget(element: Element[_]): Boolean = {
    element match {
      case a: Apply1[_, _] =>
        a.args.exists(arg => isDerivativeTarget(arg))
      case a: Apply2[_, _, _] =>
        a.args.exists(arg => isDerivativeTarget(arg))
      case a: Apply3[_, _, _, _] =>
        a.args.exists(arg => isDerivativeTarget(arg))
      case a: Apply4[_, _, _, _, _] =>
        a.args.exists(arg => isDerivativeTarget(arg))
      case a: Apply5[_, _, _, _, _, _] =>
        a.args.exists(arg => isDerivativeTarget(arg))
      case _ =>
        element == derivativeTarget
    }
  }

  def isDerivativeTarget(variable: Variable[_]): Boolean = {
    variable match {
      case ev: ElementVariable[_] => isDerivativeTarget(ev.element)
      case _ => false
    }
  }
}
