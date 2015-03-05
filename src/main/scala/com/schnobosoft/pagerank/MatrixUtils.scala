package com.schnobosoft.pagerank

import breeze.linalg.CSCMatrix
import breeze.linalg.SparseVector
import breeze.linalg.Vector


/**
 * @author Carsten Schnober
 */
object MatrixUtils {

  /**
   * Build a sparse matrix from a file expected to contain a pair of two integers in each line.
   *
   * @param location  the input file location
   * @param dimensions  the total number of nodes in the input data, i.e. the dimensionality of the resulting matrix
   * @param nLines  if given, read the first n lines of the input data only
   */
  def adjMatrix(location: String, dimensions: Int, nLines: Int = Int.MaxValue): CSCMatrix[Double] = {
    val builder = new CSCMatrix.Builder[Double](dimensions, dimensions)
    FileUtils.iteratorHead(FileUtils.fileIterator(location, nLines), nLines)
      .foreach { pair: (Int, Int) => builder.add(pair._2, pair._1, 1d) }
    builder.result
  }
  /**
   * Get the columns indices containing non-zero elements in a sparse matrix row.
   *
   * @param i the row index
   * @param m a CSCMatrix
   * @return a Sequence of column indizes containing non-zero elements in a row
   */
  def incoming(i: Int, m: CSCMatrix[Double]): Seq[Int] = {
    m.rowIndices.toList
      .zipWithIndex.filter { _._1 == i } // tuples with value matching i (row)
      .map { x => m.colPtrs.zipWithIndex.find { y => y._1 > x._2 } }
      .map { x => x.get._2 - 1 }
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
   *  Divide each cell by the number of outgoing links, i.e. the number of ones in the same column.
   *  @param m a CSCMatrix expected to contain ones and zeroes
   *  @return a column-stochastic CSCMatrix, i.e. each column adding up to 1.
   */
  def stochasticMatrix(m: CSCMatrix[Double]): CSCMatrix[Double] = {
    m.activeIterator.foreach(cell => m.update(cell._1, cell._2 / outDegree(cell._1._2, m)))
    m
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

}