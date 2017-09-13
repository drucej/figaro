package com.cra.figaro.test.algorithm.sensitivity

/**
  * Created by dscof on 6/22/2017.
  */
trait Derivative[O] {
  def getPreviousDerivatives(): List[O]
  def appendPreviousDerivative(derivative: O)
  def computeDerivative(): O
}