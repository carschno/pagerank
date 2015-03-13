package com.schnobosoft.pagerank.breeze

import scala.Range

import breeze.linalg.CSCMatrix
import breeze.linalg.DenseVector
import breeze.linalg.Vector
import breeze.linalg.norm
import breeze.linalg.sum

/**
 * Implementation of the PageRank algorithm using Scala and Breeze.
 * @author Carsten Schnober
 */
object PageRank {

  final val EPSILON = 0.0000001
  private final val BETA_DEFAULT = 0.8
  object Method extends Enumeration {
    val ITERATIVE, MATRIX = Value
  }

  /**
   * @param location  location of the input file
   * @param nPages  total number of pages (nodes)
   * @param nLines the maximum number of lines to read from input file
   * @param method the method to use (iterative vs. matrix-based)
   * @return a vector defining the PageRank value for each page/node
   */
  def pagerank(location: String, nPages: Int, nLines: Int = Int.MaxValue,
    method: Method.Value = PageRank.Method.ITERATIVE, beta: Double = BETA_DEFAULT): DenseVector[Double] = {
    val m = MatrixUtils.stochasticMatrix(MatrixUtils.adjMatrix(location, nPages, nLines))
    //    val m = MatrixUtils.adjMatrix(location, nPages, nLines)
    val rInitial = DenseVector.ones[Double](nPages) :/ nPages.toDouble

    if (method == Method.ITERATIVE) {
      println("Using iterative method.")
      computeRIterative(m, rInitial, beta = beta)
    } else if (method == Method.MATRIX) {
      println("Using matrix-based method.")
      computeRMatrix(m, rInitial, beta = beta)
    } else {
      throw new IllegalArgumentException()
    }
  }

  /**
   * Generate a vector of length n with either beta or 0 for dead ends
   * @param m a CSCMatrix
   * @param beta  the default beta value for non-dead ends
   * @return a vector containing beta or 0 for each column of m
   */
  def computeBeta(m: CSCMatrix[Double], beta: Double): Vector[Double] = {
    val v = DenseVector.fill[Double](m.cols, beta)
    for (i <- Range(0, m.cols).filter(MatrixUtils.outDegree(_, m) == 0)) {
      v.update(i, 0)
    }
    v
  }

  /**
   * Iterative implementation of PageRank
   *
   * @param m the stochastic adjacency matrix
   * @param r the initial vector R holding the PageRank values for each node/page
   * @param beta one minus the teleport probability
   * @param counter counts the number of iterations
   * @return a vector defining the PageRank value for each page/node
   */
  @deprecated("Use computeRMatrix() instead.")
  def computeRIterative(m: CSCMatrix[Double], r: DenseVector[Double], beta: Double, counter: Int = 1): DenseVector[Double] = {
    println("Iteration: " + counter)

    /* compute r' */
    val rNew = DenseVector.zeros[Double](r.size);
    for (j <- 0 until r.length) {
      MatrixUtils.incoming(j, m).foreach { i => rNew(j) += beta * (r(i) / MatrixUtils.outDegree(i, m)) }
    }

    /* re-insert r' */
    val S = sum(rNew)
    rNew :+= (1 - S) / rNew.size

    /* recursion */
    if (manhattanDistance(rNew, r) > PageRank.EPSILON)
      computeRIterative(m, rNew, beta, counter + 1)
    else
      rNew
  }

  /**
   * Matrix-based implementation of PageRank
   *
   * @param m the stochastic adjacency matrix
   * @param r the initial vector R holding the PageRank values for each node/page
   * @param beta one minus the teleport probability
   * @param counter counts the number of iterations
   * @return a vector defining the PageRank value for each page/node
   */
  def computeRMatrix(m: CSCMatrix[Double], r: DenseVector[Double], beta: Double, counter: Int = 1): DenseVector[Double] = {
    println("Iteration: " + counter)
    val rNew = (m * beta) * r :+ ((1 - beta) / m.cols)

    /* recursion */
    if (PageRank.manhattanDistance(rNew, r) > PageRank.EPSILON)
      computeRMatrix(m, rNew, beta, counter + 1)
    else
      rNew
  }

  /**
   * Compute the cosine distance between two vectors, i.e. 1 - the cosine similarity.
   *
   * @param a the first vector
   * @param b the second vector
   * @return the cosine distance between the two vectors
   */
  private def cosineDistance(a: Vector[Double], b: Vector[Double]): Double = {
    1 - (a dot b) / (norm(a, 2) * norm(b, 2))
  }

  /**
   * Compute the Manhattan distance between two vectors.
   *
   * @param a the first vector
   * @param b the second vector
   * @return the Manhatten distance between the two vectors
   */
  private def manhattanDistance(a: Vector[Double], b: Vector[Double]): Double = {
    (a - b).norm(1)
  }

}