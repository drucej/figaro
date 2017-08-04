/*
 * StructuredAlgorithm.scala
 * Algorithms for structured factored inference.
 *
 * Created By:      William Kretschmer (kretsch@mit.edu)
 * Creation Date:   Dec 21, 2016
 *
 * Copyright 2016 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 *
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */
package com.cra.figaro.algorithm.structured.algorithm

import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.ParticleGenerator
import com.cra.figaro.algorithm.structured._
import com.cra.figaro.algorithm.structured.solver.Solution
import com.cra.figaro.algorithm.structured.strategy.range.RangingStrategy
import com.cra.figaro.algorithm.structured.strategy.refine.RefiningStrategy
import com.cra.figaro.algorithm.structured.strategy.solve.SolvingStrategy
import com.cra.figaro.language._

/**
 * Structured algorithms that perform inference on a problem by a sequence of refining and solving steps. The algorithm
 * runs in a single universe.
 */
abstract class StructuredAlgorithm extends Algorithm {
  /**
   * Universe to which elements in the corresponding problem belong.
   */
  def universe: Universe

  /**
   * List of targets that should not be eliminated when solving the problem.
   * @return Targets for the problem.
   */
  def problemTargets: List[Element[_]]

  /**
   * Strategy to use for ranging atomic components. This is only called once. Uses the default ranging strategy unless
   * overwritten.
   * @return A ranging strategy to be used for ranging all atomic components.
   */
  def rangingStrategy: RangingStrategy = RangingStrategy.default(ParticleGenerator.defaultNumSamplesFromAtomics)

  /**
   * Strategy to use for refinement at a single iteration. This may return a new strategy for each iteration.
   * @return A refining strategy to be used for a single iteration.
   */
  def refiningStrategy(): RefiningStrategy

  /**
   * Strategy to use for solving at a single iteration. This may return a new strategy for each iteration.
   * @return A solving strategy to be used for a single iteration.
   */
  def solvingStrategy(): SolvingStrategy

  /**
   * Verify that all constraint factors satisfy the bounds needed for the correctness of this algorithm. This gets
   * executed before solving. By default, this method does nothing; subclasses can override this to throw an exception
   * if the bounds requirements are not met.
   */
  def checkConstraintBounds(): Unit = {}

  /**
   * All bounds for which this algorithm needs to compute solutions. This is determined by looking for components that
   * have * in their range, and have constraint factors associated with them. If such a component exists, we need both
   * lower and upper bounds. Otherwise, just one of the bounds suffices because they are equivalent; it defaults to
   * lower in this case.
   * @return All bounds for which this algorithm should compute solutions.
   */
  def neededBounds(): Set[Bounds] = {
    val hasStarConstraint = problem.components.exists(comp => comp.range.hasStar && comp.constraintFactors().nonEmpty)
    if(hasStarConstraint) Set(Lower, Upper)
    else Set(Lower)
  }

  /**
   * Extract the solution in a way that allows fast queries to the algorithm. This usually involves storing some form of
   * the solution in a variable, but the exact implementation is up to the algorithm that overrides this.
   * @param solutions A map from bounds to computed solutions for those bounds. Contains one key for each of the bounds
   * needed, according to `neededBounds()`.
   */
  def processSolutions(solutions: Map[Bounds, Solution]): Unit

  /**
   * Collection containing all components that the problem or its subproblems use.
   */
  val collection = new ComponentCollection()
  collection.rangingStrategy = rangingStrategy

  /**
   * Inference problem to be solved.
   */
  val problem = new Problem(collection, problemTargets)

  /**
   * Initialize the problem by adding all permanent elements to it. This is to ensure that all top-level elements are
   * correctly added to the top-level problem.
   */
  override def initialize(): Unit = {
    super.initialize()
    universe.permanentElements.foreach(problem.add(_))
  }

  /**
   * Run a single iteration of refinement/solving, then record the solutions.
   */
  def runStep(): Unit = {
    refiningStrategy().execute()
    checkConstraintBounds()
    val solutions = neededBounds().map(bounds => {
      solvingStrategy().execute(bounds)
      bounds -> (problem.solution, problem.recordingFactors)
    }).toMap
    processSolutions(solutions)
  }
}

trait AnytimeStructured extends StructuredAlgorithm with Anytime

trait OneTimeStructured extends StructuredAlgorithm with OneTime {
  // One time structured algorithms run refinement and solving just once each.
  override def run(): Unit = runStep()
}

/**
 * Structured algorithms that are lazy. The only method this changes is the `checkConstraintBounds` method, where it
 * requires all constraints to be in the range [0.0, 1.0].
 */
trait LazyStructured extends StructuredAlgorithm {
  override def checkConstraintBounds(): Unit = {
    // Look at the non constraint factors of the components not fully enumerated. This check is necessary because
    // components not fully enumerated might later have constraints out of bounds when their ranges increase in size.
    for(comp <- problem.components if !comp.fullyEnumerated) {
      // Verify that all entries in the factors are in the range [0.0, 1.0].
      for(factor <- comp.constraintFactors() ; indices <- factor.getIndices) {
        val entry = factor.get(indices)
        require(0.0 <= entry._1 && entry._1 <= 1.0, s"constraint for element ${comp.element} out of bounds: $entry")
      }
    }
  }
}

