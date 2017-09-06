package com.cra.figaro.test.algorithm.sensitivity

/**
  * Created by dscof on 6/22/2017.
  */
trait SearchStrategy[P] {
  def isStoppingConditionMet(): Boolean
  def nextParameterValue(P:Double): P
  // def runUntilStoppingConditionMet(): P
}
