package com.schnobosoft.pagerank

import scala.io.Source.fromFile
import breeze.linalg.CSCMatrix
import breeze.linalg.DenseVector
import breeze.linalg.Vector
import breeze.linalg.norm
import breeze.linalg.sum
import breeze.linalg.SparseVector

/**
 * Implementation of the PageRank algorithm using Scala and Breeze.
 * @author Carsten Schnober
 */
object PageRank {
  private final val COMMENT_MARKER = "#"
  private final val COLUMN_SEPARATOR = "\t"
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
    val m = stochasticMatrix(adjMatrix(location, nPages, nLines))
    val rInitial = DenseVector.ones[Double](nPages) :/ nPages.toDouble

    if (method == Method.ITERATIVE) {
      println("Using iterative method.")
      rIterative(m, rInitial)
    } else if (method == Method.MATRIX) {
      println("Using matrix-based method.")
      rMatrix(m, rInitial)
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
  @deprecated("Use rMatrix instead.")
  private def rIterative(m: CSCMatrix[Double], r: DenseVector[Double], beta: Double = 0.8, counter: Int = 1): DenseVector[Double] = {
    println("Iteration: " + counter)

    /* compute r' */
    val rNew = DenseVector.zeros[Double](r.size);
    for (j <- Range(0, r.length)) {
      incoming(j, m).foreach { i => rNew(j) += beta * (r(i) / outDegree(i, m)) }
    }

    /* re-insert r' */
    val S = sum(rNew)
    rNew :+= (1 - S) / rNew.size

    /* recursion */
    if (manhattanDistance(rNew, r) > EPSILON)
      rIterative(m, rNew, beta, counter + 1)
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
  private def rMatrix(m: CSCMatrix[Double], r: DenseVector[Double], beta: Double = 0.8, counter: Int = 1): DenseVector[Double] = {
    println("Iteration: " + counter)
    val rNew = (m * beta) * r + ((1 - beta) / m.cols)

    /* recursion */
    if (manhattanDistance(rNew, r) > EPSILON)
      rMatrix(m, rNew, beta, counter + 1)
    else
      rNew
  }

  /**
   * Get the rows containing non-zero elements in a sparse matrix column.
   * @param i the column index
   * @param a CSCMatrix
   * @return an array of row indices for the given column i.
   */
  def outgoing(i: Int, m: CSCMatrix[Double]): Array[Int] = {
    m.rowIndices.slice(m.colPtrs(i), m.colPtrs(i + 1))
  }

  /**
   * Compute the number of non-zero elements in a sparse matrix column.
   * @param i the column number
   * @param m a CSCMatrix
   * @return the number of outgoing links with regard to the given column
   */
  def outDegree(i: Int, m: CSCMatrix[Double]): Int = {
    m.colPtrs(i + 1) - m.colPtrs(i)
  }

  /**
   * Get the columns indices containing non-zero elements in a sparse matrix row.
   *
   * @param i the row index
   * @param m a CSCMatrix
   * @return a Sequence of column indizes containing non-zero elements in a row
   */
  @deprecated("Used in iterative approach only.")
  def incoming(i: Int, m: CSCMatrix[Double]): Seq[Int] = {
    m.rowIndices.toList
      .zipWithIndex.filter { _._1 == i } // tuples with value matching i (row)
      .map { x => m.colPtrs.zipWithIndex.find { y => y._1 > x._2 } }
      .map { x => x.get._2 - 1 }
  }

  /**
   * Compute the number of non-zero elements in a sparse matrix row.
   *
   * @param i the row index
   * @param m a CSCMatrix
   * @return the number of columns containing non-zero elements in the given row.
   */
  @deprecated("Used in iterative approach only.")
  def inDegree(i: Int, m: CSCMatrix[Double]): Int = {
    m.rowIndices.toList.count { _ == i }
  }

  /**
   * Get an iterator over the lines of a file. Each line is expected to contain a pair of two integers,
   * separated by a TAB. Comments are ignored. The iterator thus returns tuples of integers.
   *
   * @param location  the location of the input file
   * @param nLines  if given, read this number of lines only
   * @return an iterator over tuples of two integers
   */
  private def fileIterator(location: String, nLines: Int = Int.MaxValue): Iterator[(Int, Int)] = {
    iteratorHead(fromFile(location).getLines
      .filter { !_.startsWith(COMMENT_MARKER) }, nLines) // ignore comments
      .map { (_.split(COLUMN_SEPARATOR)) } // split line
      .map { (x => (Integer.parseInt(x(0)), Integer.parseInt(x(1)))) } // convert to Integers
  }

  /**
   * Reduce an iterator of definite size to its n leading entries. If the iterator does not have a
   * definite size or its size is smaller than the given n, return it unchanged.
   *
   * @param iter an iterator
   * @param n the maximum number of entries to return for the iterator
   * @return an iterator with the given number of entries max.
   */
  private def iteratorHead[T](iter: Iterator[T], n: Int): Iterator[T] = {
    if (iter.hasDefiniteSize && iter.size < n)
      iter
    else
      iter.take(n)
  }

  /**
   * Build a sparse matrix from a file expected to contain a pair of two integers in each line.
   *
   * @param location  the input file location
   * @param dimensions  the total number of nodes in the input data, i.e. the dimensionality of the resulting matrix
   * @param nLines  if given, read the first n lines of the input data only
   */
  def adjMatrix(location: String, dimensions: Int, nLines: Int = Int.MaxValue): CSCMatrix[Double] = {
    val builder = new CSCMatrix.Builder[Double](dimensions, dimensions)
    iteratorHead(fileIterator(location, nLines), nLines)
      .foreach { pair: (Int, Int) => builder.add(pair._2, pair._1, 1d) }
    builder.result
  }

  /**
   *  Divide each cell by the number of outgoing links, i.e. the number of ones in the same column.
   *  @param m a CSCMatrix expected to contain ones and zeroes
   *  @return a column-stochastic CSCMatrix, i.e. each column adding up to 1.
   */
  def stochasticMatrix(m: CSCMatrix[Double]): CSCMatrix[Double] = {
    m.activeIterator.foreach(cell => m.update(cell._1, cell._2 / outDegree(cell._1._2, m)))
    m
  }

  /** Return the sum over all values in a column */
  @deprecated("No longer used.")
  def colSum(i: Int, m: CSCMatrix[Double]): Double = {
    m(0 to m.rows - 1, i to i).sum
  }

  /** Get the sums of each column and return as a sparse vector */
  @deprecated("No longer used.")
  def colSums(m: CSCMatrix[Double]): Vector[Double] = {
    val sums = SparseVector.zeros[Double](m.cols)
    m.activeIterator.foreach { x => sums(x._1._2) += x._2 }
    sums
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