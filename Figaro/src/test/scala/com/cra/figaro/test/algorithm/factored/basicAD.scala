package com.cra.figaro.test.algorithm.factored

import com.cra.figaro.library.compound.If
import com.cra.figaro.language.Flip
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.AutomaticDifferentiation.AutomaticDifferentiationVariableElimination
import com.cra.figaro.algorithm.AutomaticDifferentiation.AutoDiffVariableElimination
import com.cra.figaro.algorithm.structured.algorithm.structured.StructuredVE
import com.cra.figaro.language._


object basicAD {

  val pThreat = 0.25;
  val pIndGivenThreat = Constant(0.8);
  val pIndGivenNotThreat = Constant(0.1);
  val pAlertGivenInd = 0.75;
  val pAlertGivenNotInd = 0.2;

  val Threat = Flip(pThreat)
  val Ind = Flip(If(Threat,pIndGivenThreat,pIndGivenNotThreat))
//  val Alert = Flip(If(Ind,pAlertGivenInd,pAlertGivenNotInd))
  //  Alert.observe(true)

  def main(args: Array[String]) {

    println(StructuredVE.probability(Ind, true))
//    val alg = AutoDiffVariableElimination(List(pIndGivenThreat),Ind)
//    alg.start()
//    println(alg.expectation(Ind, (x:Boolean) => if (x) 1.0 else 0.0))
//    println(alg.semiring.sum(alg.semiring.product((1.0,0.0),(0.332,0.332)),alg.semiring.product((0.0,0.0),(0.668,0.668))))
//    alg.kill


    //  val ve = VariableElimination(Alert)
    //  ve.start()
    //  ve.distribution(Alert)
    ////  ve.probability(Alert, (b:Boolean) => true)
    //  ve.kill()

  }
}

