/*
 * Semiring.scala
 * Sum and product operations according to a semiring algebraic structure.
 * 
 * Created By:      Michael Howard (mhoward@cra.com)
 * Creation Date:   Jun 3, 2013
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

package com.cra.figaro.algorithm.factored.factors

/**
 * Operations in factored algorithms are defined by a semiring algebraic structure.
 * Each semiring defines a product and sum operation, and a value for zero and one which satisfy a set of properties.
 * Different semirings are appropriate for certain algorithms and data types.
 */
//trait SemiringDual[T] {
//  /**
//   * Product of two entries, as defined by the particular inference problem.
//   */
//  def product(x: T, y: T): T
//
//  /**
//   * Sum of two entries, as defined by the particular inference problem. E.g., for computing conditional probabilities,
//   * sum is ordinary addition; for most probable explanation, it is max.
//   */
//  def sum(x: T, y: T): T
//
//  /**
//   * Sum of many entries. Typically, this would be implemented by the ordinary sum,
//   * but there may be more efficient implementations.
//   */
//  def sumMany(xs: Traversable[T]): T = {
//    xs.foldLeft(zero)(sum(_, _))
//  }
//
//  /**
//   * A value such that a + 0 = a
//   */
//  val zero: T
//  /**
//   * A value such that a*1 = a
//   */
//  val one: T
//
//}
//
//trait ArithmeticSemiRing[T] extends SemiringDual[T] {
//  /**
//   * Division of two entries, as defined by the particular inference problem.
//   */
//  def divide(x: T, y: T): T
//}

case class SumProductDualSemiring() extends DivideableSemiRing[(Double, Double)] {
  /**
   * Multiplying with dual numbers.
   */
  def product(x: (Double, Double), y: (Double, Double)) = {
    val v = (x._1 * y._1, x._1 * y._2 + x._2 * y._1)
    println(x + "*" + y  + "=" + v)
    v
  }

  /**
   * Dividing with dual numbers.
   */
  def divide(x: (Double, Double), y: (Double, Double)) = {
    if (y._1 == 0) {
      println("0.0")
      (0.0, 0.0)
    }
//    val v = (x._1 / y._1,  x._2 / y._2)   //test!!!
    val v = (x._1 / y._1,  (x._2 * y._1 - x._1 * y._2) / (y._1 * y._1)) 

    println(x + "/" + y + "=" + v)
    v
  }
 

  /**
   * Decision joint factor marginalization.
   */
  def sum(x: (Double, Double), y: (Double, Double)) = {
    val v = (x._1 + y._1 , x._2 + y._2)
    println(x + "+" + y + "=" + v)
    v
  }

  /**
   * 0 probability and 0 dual.
   */
  val zero = (0.0, 0.0)
  /**
   * 1 probability and 0 dual.
   */
  val one = (1.0, 0.0)

}

//case class BooleanSemiring() extends Semiring[Boolean] {
//  /**
//   * x AND y
//   */
//  def product(x: Boolean, y: Boolean): Boolean = x && y
//  /**
//   * x OR y
//   */
//  def sum(x: Boolean, y: Boolean): Boolean = x || y
//  /**
//   * False
//   */
//  val zero = false
//  /**
//   * True
//   */
//  val one = true
//}
//
////case class SumProductSemiring() extends DivideableSemiRing[Double] {
////  /**
////   * Standard multiplication
////   */
////  def product(x: Double, y: Double) = x* y
////
////  /**
////   * Standard division
////   */
////  def divide(x: Double, y: Double) = if (y == zero) zero else x / y
////
////  /**
////   * Standard addition
////   */
////  def sum(x: Double, y: Double) = x + y
////  /**
////   * 0
////   */
////  val zero = 0.0
////  /**
////   * 1
////   */
////  val one = 1.0
////}
//
///**
// * Semiring for computing sums and products with log probabilities.
// */
//case class LogSumProductSemiring() extends DivideableSemiRing[Double] {
//  val zero = Double.NegativeInfinity
//
//  val one = 0.0
//
//  def product(x: Double, y: Double) = x + y
//
//  def divide(x: Double, y: Double) = if (y == zero) zero else x - y
//
//  override def sumMany(xs: Traversable[Double]): Double = {
//    val max = xs.foldLeft(Double.NegativeInfinity)(_ max _)
//    if (max == Double.NegativeInfinity) Double.NegativeInfinity
//    else {
//      var total = 0.0
//      for (x <- xs) { total += Math.exp(x - max) }
//      Math.log(total) + max
//    }
//  }
//
//  def sum(x: Double, y: Double) = sumMany(List(x, y))
//}
//
///**
// * Semiring for computing maxs and products with log probabilities.
// */
//case class LogMaxProductSemiring() extends DivideableSemiRing[Double] {
//  val zero = Double.NegativeInfinity
//
//  val one = 0.0
//
//  def product(x: Double, y: Double) = x + y
//
//  def divide(x: Double, y: Double) = if (y == zero) zero else x - y
//
//  def sum(x: Double, y: Double) = x max y
//}
//
///**
// * Semiring for computing sums and products with lower and upper bounds.
// */
//case class BoundsSumProductSemiring() extends DivideableSemiRing[(Double, Double)] {
//  def product(x: (Double, Double), y: (Double, Double)) = {
//    val (lx, ux) = x
//    val (ly, uy) = y
//    (lx * ly, ux * uy)
//  }
//
//  def divide(x: (Double, Double), y: (Double, Double)) = {
//    val (lx, ux) = x
//    val (ly, uy) = y
//    (if (ly == zero._1) zero._1 else lx / ly, if (uy == zero._2) zero._2 else ux / uy)
//  }
//
//  def sum(x: (Double, Double), y: (Double, Double)) = {
//    val (lx, ux) = x
//    val (ly, uy) = y
//    (lx + ly, ux + uy)
//  }
//
//  val zero = (0.0, 0.0)
//
//  val one = (1.0, 1.0)
//}
//
//case class MaxProductSemiring() extends DivideableSemiRing[Double] {
//  /**
//   * Standard multiplication
//   */
//  def product(x: Double, y: Double) = x * y
//
//  /**
//   * Standard division
//   */
//  def divide(x: Double, y: Double) = if (y == zero) zero else x / y
//
//  /**
//   *
//   * The maximum of x and y.
//   */
//  def sum(x: Double, y: Double) = x max y
//  /**
//   * 0
//   */
//  val zero = 0.0
//  /**
//   * 1
//   */
//  val one = 1.0
//}

