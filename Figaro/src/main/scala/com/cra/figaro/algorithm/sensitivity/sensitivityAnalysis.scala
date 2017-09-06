package com.cra.figaro.test.algorithm.sensitivity

import com.cra.figaro.language.Flip
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.{ADVariableElimination, VariableElimination}
import com.cra.figaro.algorithm.filtering.ParticleFilter
import com.cra.figaro.test.algorithm.adfactored.simpleTest//.{Alert, pAlertGivenInd}
//import com.cra.figaro.algorithm.AutomaticDifferentiation.AutoDiffVariableElimination
//import com.cra.figaro.algorithm.AutomaticDifferentiation.AutoDiffVariableEliminationDan
import com.cra.figaro.language._
import scala.collection.mutable.ListBuffer




/**
  * The purpose of this code is to compute the sensitivity of a a model with respect to one of its input
  * parameters. In this particular case, we are interested in a simple 2-node model (Threat)->(Indicator).
  * In this sensitivity analysis, we are interested in the minimum change (d*) to a parameter (x) at a specific
  * value (x_0), in order to change a query (f(x)) by a given amount (e),
  * The sensitivity analysis can be formulated as a simple minimization problem:
  *         d* = argmin d
  *        s.t. |f(x_0 + d)-f(x_0)|>e
  * where x_0 and e are given by the user. We solve this by using Newton's zero finding method, while obtaining
  * the gradient from automatic differentiation.
  *
  * Note: To use a zero finding method, we must subtract the f(x_0) and e from the query function f(x) in order to
  * obtain the correct parameter value. I refer to the function to zero as the cost function, and the original
  * query function as the query.
  */

// class gives a blueprint for a concrete object (what is a singleton?) (why is an object static?)

class ADVESensitivityModel extends SensitivityModel[Double, Double] {

  var prevParamValues = ListBuffer.empty[Double]
  var prevQueryFunctionValues = ListBuffer.empty[(Double, Double)]
  var prevCostFunctionValues = ListBuffer.empty[Double]
  var prevDerivitiveValues = ListBuffer.empty[Double]

  // var stuff = Array[Array[Double]]
  // var stuff = ListBuffer[MyCaseThing]
  // case class MyCaseThing(parValue: DOuble, qfv: (Double, Double), ...etc.)

  // Here I define a series of methods to retrieve (getxxx) and tack on (appendxx) parameter values, query outputs,
  // and cost function outputs.
  // This is kind of ugly... Can I do this elsewhere?

  def getPreviousParameterValues(): List[Double] =
  {
    return prevParamValues.toList
  }

  def getPreviousQueryFnValues(): List[(Double, Double)] =
  {
    return prevQueryFunctionValues.toList
  }

  def getPreviousCostFnValues(): List[Double] =
  {
    return prevCostFunctionValues.toList
  }

  def getPreviousDerivatives(): List[Double] =
  {
    return prevDerivitiveValues.toList
  }
  //////////////

  def appendParameterValue(value: Double): Unit =
  {
    prevParamValues.append(value)
  }

  def appendQueryFnValue(value: (Double, Double)): Unit =
  {
    prevQueryFunctionValues.append(value)
  }

  def appendCostFnValue(value: (Double)): Unit =
  {
    prevCostFunctionValues.append(value)
  }

  def appendDerivitiveValue(value: Double): Unit =
  {
    prevDerivitiveValues.append(value)
  }

  def nextQueryFnValue(): (Double, Double) =
  {
    val (fn, dfn) = QueryFunction(prevParamValues.last)
    val pair = (fn, dfn)
    //      prevQueryFunctionValues.append(pair)
    pair
  }

  //
  def QueryFunction(val_pIndGivenThreat: Double): (Double, Double) = {
    //  def QueryFunction(val_pIndGivenThreat: Double): (Double) = {

    val pThreat = 0.25;
    val pIndGivenThreat = Constant(val_pIndGivenThreat);
    val pIndGivenNotThreat = Constant(0.15);

    val Threat = Flip(pThreat)
    val Ind = Flip(If(Threat, pIndGivenThreat, pIndGivenNotThreat))

    val alg1 = ADVariableElimination.debugged(pIndGivenThreat, Ind)


    alg1.start()
    //    val ret = alg.expectation(Ind, (x:Boolean) => if (x) 1.0 else 0.0)
//    val ret2 = alg1.computeExpectation(Ind, (x: Boolean) => if (x) 1.0 else 0.0)
    val (prob, deriv) = alg1.getProbabilityAndDerivative(Ind, true)

    val answerHolder = (prob, deriv) // Place holder for true output of AD

    alg1.kill()
    return answerHolder
  }

  /*   Given initial parameter value x_0, we want to find x s.t. |f(x) - f(x_0)| = percentChange
   *   To cast this as a zero finding problem, move percentChange to LHS to get
   *  |f(x) - f(x_0)| - percent change = 0
   */
  def nextCostFnValue( percentChange : Double) : Double =
  {

    val distToTargetQueryValue = QueryFunction(prevParamValues.last)._1 - QueryFunction(prevParamValues.head)._1 - percentChange*QueryFunction(prevParamValues.head)._1
    return distToTargetQueryValue
  }

}

// Example -> Newton's Method
class ExampleSearchStrategyV2(model: SensitivityModel[Double, Double]) extends SearchStrategy[Double] {
  // needs to be modified to reasonable stopping condition
  def isStoppingConditionMet(): Boolean = {
    math.abs((model.getPreviousQueryFnValues().last)._1) < 0.00001
    //math.abs((model.getPreviousQueryFnValues().head - model.getPreviousParameterValues().last)/model.getPreviousQueryFnValues().head) >   .10
  }

  def nextParameterValue(percentChange: Double): Double = {

    // needs to be absolute value
    // percent change needs to be multiplied by initial value

    //    val previousCost = (model.getPreviousQueryFnValues().last)._1 - (model.getPreviousQueryFnValues().head)._1 - percentChange
    //    val Cost = nextCostFnValue(percentChange)
    val cost = model.getPreviousCostFnValues().last

    //        println("prev param value : " + model.getPreviousParameterValues().last + " , prev Cost val : " + model.getPreviousCostFnValues().last)
    val paramterShift = (cost / model.getPreviousDerivatives().last)
    //    println("paramterShift : " + paramterShift)
    model.getPreviousParameterValues().last - paramterShift
  }
}

object SensitivityJeff {

  // Get initial values:
  def main(args: Array[String]): Unit = {
    // Get initial values

    // Why can I not use the previously defined parameters?

    //    val sa = SensitivityUserFacing
    //
    val initialParamValue = 0.7
    val percentChange = 0.1
    val fn = new ADVESensitivityModel()
    val fn0 = fn.QueryFunction(initialParamValue)

    fn.appendParameterValue(initialParamValue)
    fn.appendQueryFnValue(fn0)

    val cFn0 = fn.nextCostFnValue(percentChange)

    fn.appendCostFnValue(cFn0)

    val dfn = (fn.prevQueryFunctionValues.head)._2 //new ExampleADDerivative(fn)
    val newton = new ExampleSearchStrategyV2(fn)

    var n = 1
    fn.appendDerivitiveValue(dfn)

    println("x0 = " + initialParamValue + ", f(x0) = " + fn0 + ", Cost Value : " + fn.getPreviousCostFnValues().last )
    //    println("prevParameterValues" + prevParamValues)

    //    while (newton.isStoppingConditionMet())

    for (a <- 1 to 10) {
      val dfn = (fn.prevQueryFunctionValues.last)._2 //new ExampleADDerivative(fn)

      val xn = newton.nextParameterValue(percentChange)
      if (a != 1) {
        fn.appendParameterValue(xn)
      }

      fn.appendQueryFnValue(fn.nextQueryFnValue())
      fn.appendCostFnValue(fn.nextCostFnValue(percentChange))
      fn.appendDerivitiveValue(dfn)

      //      val costFunction = (x:Double , y:Double, z:Double) => x - y - z
      //      val cost = costFunction((fn.getPreviousQueryFnValues().last)._1, (fn.getPreviousQueryFnValues().head)._1 , percentChange )
      //      println("X( " , n, " = ", xn, "Query Function = ", fn.nextQueryFnValue())
      println("Iteration number = " + n + ", test parameter value = " + xn + " , Query Value = " + fn.getPreviousQueryFnValues().last + "Derivitive Value = " + fn.getPreviousDerivatives().last + " , Cost Function (Should go to zero) = " + fn.getPreviousCostFnValues().last )
      n += 1
    }
  }

  // thing1val
  // thing2val...

  // do the loop
  //    use thing1val and thing2val and create newthing1val and newthing2val
  //    store old thing1val and thing2val
  //    set thing1val=newthing1val etc.

}