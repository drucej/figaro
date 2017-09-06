package com.cra.figaro.test.algorithm.sensitivity

/**
  * Created by dscof on 6/22/2017.
  */
trait SensitivityModel[P, O] {
  def getPreviousParameterValues(): List[P]
  def getPreviousQueryFnValues(): List[(O,O)]
  def getPreviousCostFnValues(): List[O]
  def getPreviousDerivatives(): List[O]
  def appendParameterValue(value: P)
  def appendQueryFnValue(value: (O,O))
  def nextQueryFnValue(): (O,O)
  //  def nextParameterValue(): P
}
