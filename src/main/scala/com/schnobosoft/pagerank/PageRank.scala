package com.schnobosoft.pagerank

import scala.io.Source.fromFile

import breeze.linalg.CSCMatrix
import breeze.linalg.DenseVector
import breeze.linalg.SparseVector
import breeze.linalg.Vector
import breeze.linalg.norm
import breeze.linalg.sum

/**
 * Implementation of the PageRank algorithm using Scala and Breeze.
 * @author Carsten Schnober
 */
object PageRank {

  final val EPSILON = 0.00001
  private final val BETA = 0.8
  object Method extends Enumeration {
    val ITERATIVE, MATRIX = Value
  }

  /**
   * @param location  location of the input file
   * @param nPages  total number of pages (nodes)
   * @param nLines: the maximum number of lines to read from input file
   * @param method: the method to use (iterative vs. matrix-based)
   * @return a vector defining the PageRank value for each page/node
   */
  def pagerank(location: String, nPages: Int, nLines: Int = Int.MaxValue,
    method: Method.Value = PageRank.Method.ITERATIVE): DenseVector[Double] = {
    val m = MatrixUtils.stochasticMatrix(MatrixUtils.adjMatrix(location, nPages, nLines))
    val rInitial = DenseVector.ones[Double](nPages) :/ nPages.toDouble

    if (method == Method.ITERATIVE) {
      println("Using iterative method.")
      computeRIterative(m, rInitial)
    } else if (method == Method.MATRIX) {
      println("Using matrix-based method.")
      computeRMatrix(m, rInitial)
    } else {
      throw new IllegalArgumentException()
    }
  }

  /**
   * Iterative implementation of PageRank
   *
   * @param m the stochastic adjacency matrix
   * @param r the initial vector R holding the PageRank values for each node/page
   * @param beta the teleport probability
   * @param counter counts the number of iterations
   * @return a vector defining the PageRank value for each page/node
   */
  @deprecated("Use computeRMatrix() instead.")
  def computeRIterative(m: CSCMatrix[Double], r: DenseVector[Double], beta: Double = 0.8, counter: Int = 1): DenseVector[Double] = {
    println("Iteration: " + counter)

    /* compute r' */
    val rNew = DenseVector.zeros[Double](r.size);
    for (j <- Range(0, r.length)) {
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
   * @param beta the teleport probability
   * @param counter counts the number of iterations
   * @return a vector defining the PageRank value for each page/node
   */
  def computeRMatrix(m: CSCMatrix[Double], r: DenseVector[Double], beta: Double = 0.8, counter: Int = 1): DenseVector[Double] = {
    println("Iteration: " + counter)
    val rNew = (m * beta) * r + ((1 - beta) / m.cols)

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
  def cosineDistance(a: Vector[Double], b: Vector[Double]): Double = {
    1 - (a dot b) / (norm(a, 2) * norm(b, 2))
  }

  /**
   * Compute the Manhattan distance between two vectors.
   *
   * @param a the first vector
   * @param b the second vector
   * @return the Manhatten distance between the two vectors
   */
  def manhattanDistance(a: Vector[Double], b: Vector[Double]): Double = {
    (a - b).norm(1)
  }

}