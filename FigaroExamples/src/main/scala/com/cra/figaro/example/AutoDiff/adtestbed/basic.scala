//package com.cra.adtestbed
//
//import com.cra.figaro.language.Flip
//import com.cra.figaro.library.compound.If
//import com.cra.figaro.algorithm.factored.VariableElimination
//import com.cra.figaro.algorithm.AutomaticDifferentiation.AutomaticDifferentiationVariableElimination
//import com.cra.figaro.algorithm.AutomaticDifferentiation.AutoDiffVariableElimination
//import com.cra.figaro.language._
//
//
//object basic {
//  
//  val universe = Universe.universe
//  
//  val pThreat = 0.25;
//  val pIndGivenThreat = Constant(0.9)("pIndGivenThreat",universe);
//  val pIndGivenNotThreat = Constant(0.15)("pIndGivenNotThreat",universe);
////  val pAlertGivenInd = 0.75;
//  val pAlertGivenInd = Constant(0.75)("pAlertGivenInd",universe);
//  val pAlertGivenNotInd = Constant(0.2)("pAlertGivenNotInd",universe);
//  
//  val Threat = Flip(pThreat)("Threat",universe);
//  val Ind = Flip(If(Threat,pIndGivenThreat,pIndGivenNotThreat))("Ind",universe);
////  val Ind = Flip(If(Threat,pIndGivenThreat,pIndGivenNotThreat))("Ind",universe);
////  val Alert = If(Ind,pAlertGivenInd,pAlertGivenNotInd)("Alert",universe);
//  val Alert  = If(Ind,Flip(pAlertGivenInd)("flipAlert",universe),Flip(pAlertGivenNotInd))("Alert",universe);
//
//  Threat.observe(true)
//  
//  def main(args: Array[String]) {
//
//  val alg = AutoDiffVariableElimination(List(pIndGivenThreat),Alert)
//  alg.start()
////  println(alg.expectation(Alert, (x:Boolean) => if (x) 1.0 else 0.0))
////  println(alg.semiring.sum(alg.semiring.product((1.0,0.0),(0.332,0.332)),alg.semiring.product((0.0,0.0),(0.668,0.668))))
//  alg.kill
//  
// 
////  val ve = VariableElimination(Alert)
////  ve.start()
////  ve.distribution(Alert)
////  ve.probability(Alert, (b:Boolean) => true)
////  ve.kill()
//    
//  }
//}