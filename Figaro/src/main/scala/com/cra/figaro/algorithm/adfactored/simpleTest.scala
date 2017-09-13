package com.cra.figaro.test.algorithm.adfactored

import com.cra.figaro.language.Flip
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.{ADVariableElimination, VariableElimination}
import com.cra.figaro.algorithm.filtering.ParticleFilter
//import com.cra.figaro.algorithm.AutomaticDifferentiation.AutoDiffVariableElimination
//import com.cra.figaro.algorithm.AutomaticDifferentiation.AutoDiffVariableEliminationDan
import com.cra.figaro.language._


object simpleTest {
  
  val universe = Universe.universe
  
//  val test1 = Constant(0.4);
//  val param = Constant(0.7)
//  val test2 = If(Flip(param), Constant(1), Constant(2))
//  
//  val pThreat = Constant(0.25)("pThreat",universe);
//  val pIndGivenThreat = Constant(0.9)("pIndGivenThreat",universe);
//  val pIndGivenNotThreat = Constant(0.15)("pIndGivenNotThreat",universe);
//
//  val Threat = Flip(pThreat)("Threat",universe);
//  val Ind = Flip(If(Threat,pIndGivenThreat,pIndGivenNotThreat))("Ind",universe);

  val pThreat = Constant(0.25)
  val pIndGivenThreat = Constant(0.7)
  val pIndGivenNotThreat = Constant(0.15)
  val pAlertGivenInd = Constant(0.9)
  val pAlertGivenNotInd = Constant(0.12)

  val Threat = Flip(pThreat)
//  val Ind = Flip(If(Threat, pIndGivenThreat, pIndGivenNotThreat))
  val Ind = Select(0.2 -> 0.2, 0.3 -> 0.7, 0.5 -> 0.15)
//  val Alert = Flip(If(Ind, pAlertGivenInd, pAlertGivenNotInd))
  val Alert = Flip(Ind)
  
  def main(args: Array[String]) {

  val alg = ADVariableElimination.debugged(pThreat, Alert)
    alg.start()
    val (prob, deriv) = alg.getProbabilityAndDerivative(Alert, true)
    alg.kill()
//  alg1.kill
//  
//  val alg2 = VariableElimination(Ind)
//  alg2.start()
//  alg2.kill
  println("Query answer is : " + prob + ", Derivitive value is : " + deriv)

  }
}