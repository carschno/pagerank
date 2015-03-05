package com.schnobosoft.pagerank

import breeze.linalg.DenseVector
import breeze.linalg.CSCMatrix
import org.junit.Test
import org.junit.Assert._

/**
 * @author Carsten Schnober
 */
class MatrixUtilsTest {
  var location = "src/test/resources/test1.txt"
  var location2 = "src/test/resources/test2.txt"
  var nPages = 3

  @Test
  def adjMatrixTest {
    val builder = new CSCMatrix.Builder[Double](3, 3)
    builder.add(0, 0, 1)
    builder.add(1, 0, 1)
    builder.add(0, 1, 1)
    builder.add(2, 1, 1)
    builder.add(1, 2, 1)
    val expectedM = builder.result()
    val m = MatrixUtils.adjMatrix(location, nPages)
    assertArrayEquals(expectedM.data, m.data, PageRank.EPSILON)
  }

  @Test
  def stochasticMatrixTest1 {
    val builder = new CSCMatrix.Builder[Double](3, 3)
    builder.add(0, 0, 0.5)
    builder.add(1, 0, 0.5)
    builder.add(0, 1, 0.5)
    builder.add(2, 1, 0.5)
    builder.add(1, 2, 1)
    val expectedM = builder.result()
    val m = MatrixUtils.stochasticMatrix(MatrixUtils.adjMatrix(location, nPages))
    assertArrayEquals(expectedM.data, m.data, PageRank.EPSILON)

    Range(0, 2).foreach { col => assertEquals(1.0, MatrixUtils.colSum(col, m), 0.001) }
  }

  @Test
  def stochasticMatrixTest2 {
    val builder = new CSCMatrix.Builder[Double](3, 3)
    builder.add(0, 0, 0.5)
    builder.add(1, 0, 0.5)
    builder.add(0, 1, 0.5)
    builder.add(2, 1, 0.5)
    builder.add(2, 2, 1)
    val expectedM = builder.result()
    val m = MatrixUtils.stochasticMatrix(MatrixUtils.adjMatrix(location2, nPages))
    assertArrayEquals(expectedM.data, m.data, PageRank.EPSILON)
    Range(0, 2).foreach { col => assertEquals(1.0, MatrixUtils.colSum(col, m), 0.001) }

  }

  @Test
  def incomingTest0 {
    val expectedIncoming = Array[Int](0, 1)
    val m = MatrixUtils.adjMatrix(location, nPages)
    val incoming0 = MatrixUtils.incoming(0, m)
    assertArrayEquals(expectedIncoming, incoming0.toArray)
  }

  @Test
  def incomingTest1 {
    val expectedIncoming = Array[Int](0, 2)
    val m = MatrixUtils.adjMatrix(location, nPages)
    val incoming = MatrixUtils.incoming(1, m)
    assertArrayEquals(expectedIncoming, incoming.toArray)
  }

  @Test
  def incomingTest2 {
    val expectedIncoming = Array[Int](1)
    val m = MatrixUtils.adjMatrix(location, nPages)
    val incoming = MatrixUtils.incoming(2, m)
    assertArrayEquals(expectedIncoming, incoming.toArray)
  }

  @Test
  def outgoingTest0 {
    val expected = Array[Int](0, 1)
    val m = MatrixUtils.adjMatrix(location, nPages)
    val outgoing = MatrixUtils.outgoing(0, m)
    assertArrayEquals(expected, outgoing)
  }

  @Test
  def outgoingTest1 {
    val expected = Array[Int](0, 2)
    val m = MatrixUtils.adjMatrix(location, nPages)
    val outgoing = MatrixUtils.outgoing(1, m)
    assertArrayEquals(expected, outgoing)
  }

  @Test
  def outgoingTest2 {
    val expected = Array[Int](1)
    val m = MatrixUtils.adjMatrix(location, nPages)
    val outgoing = MatrixUtils.outgoing(2, m)
    assertArrayEquals(expected, outgoing)
  }

  @Test
  def colSumTest1 {
    val expectedSum = Seq[Double](2, 2, 1)
    val m = MatrixUtils.adjMatrix(location, nPages)
    Range(0, 2).foreach { col: Int => assertEquals(expectedSum(col), MatrixUtils.colSum(col, m), 0.0001) }
  }

  @Test
  def colSumTest2 {
    val expectedSum = Seq[Double](2, 2, 1)
    val m = MatrixUtils.adjMatrix(location2, nPages)
    Range(0, 2).foreach { col: Int => assertEquals(expectedSum(col), MatrixUtils.colSum(col, m), 0.0001) }
  }

  @Test
  def allSumsTest1 {
    val expected = DenseVector[Double](2, 2, 1)
    val m = MatrixUtils.adjMatrix(location, nPages)
    val sums = MatrixUtils.colSums(m)
    assertArrayEquals(expected.data, sums.values.iterator.toArray, 0.001)
  }

  @Test
  def inDegreeTest {
    val expected = List[Int](2, 2, 1)
    val m = MatrixUtils.adjMatrix(location, nPages)
    for (i <- Range(0, 2)) {
      val in = MatrixUtils.inDegree(i, m)
      assertEquals(expected(i), in)
    }
  }
}