package com.schnobosoft.pagerank

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Test
import breeze.linalg.CSCMatrix
import breeze.linalg.DenseVector

/**
 * @author Carsten Schnober
 */

class PageRankTest {
  var location = "src/test/resources/test1.txt"
  var location2 = "src/test/resources/test2.txt"
  var nPages = 3

  @Test
  def pageRankTest {
    var expepectedR = DenseVector[Double](7d / 33d, 5d / 33d, 21d / 33d)
    var r = PageRank.pagerank(location2, 3)
    assertArrayEquals(expepectedR.data, r.data, PageRank.EPSILON)
  }

  @Test
  def pageRankTestNLines {
    var expepectedR = DenseVector[Double](7d / 33d, 5d / 33d, 21d / 33d)
    var r = PageRank.pagerank(location2, 3, 5)
    assertArrayEquals(expepectedR.data, r.data, PageRank.EPSILON)
  }

  @Test
  def iteratorMaxTest1 {
    val expected = 5
    val iterator = Iterator.from(0).take(10)
    val result = PageRank.iteratorHead(iterator, 5)
    assertEquals(expected, result.size)
  }

  @Test
  def iteratorMaxTest2 {
    val expected = 10
    val iterator = Iterator.from(0).take(10)
    val result = PageRank.iteratorHead(iterator, 15)
    assertEquals(expected, result.size)
  }

  @Test
  def iteratorMaxTest3 {
    val expected = 5
    val iterator = Iterator.from(0)
    val result = PageRank.iteratorHead(iterator, 5)
    assertEquals(expected, result.size)
  }

  @Test
  def adjMatrixTest {
    val builder = new CSCMatrix.Builder[Double](3, 3)
    builder.add(0, 0, 1)
    builder.add(1, 0, 1)
    builder.add(0, 1, 1)
    builder.add(2, 1, 1)
    builder.add(1, 2, 1)
    val expectedM = builder.result()
    val m = PageRank.adjMatrix(location, nPages)
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
    val m = PageRank.stochasticMatrix(PageRank.adjMatrix(location, nPages))
    assertArrayEquals(expectedM.data, m.data, PageRank.EPSILON)

    Range(0, 2).foreach { col => assertEquals(1.0, PageRank.colSum(col, m), 0.001) }
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
    val m = PageRank.stochasticMatrix(PageRank.adjMatrix(location2, nPages))
    assertArrayEquals(expectedM.data, m.data, PageRank.EPSILON)
    Range(0, 2).foreach { col => assertEquals(1.0, PageRank.colSum(col, m), 0.001) }

  }

  @Test
  def incomingTest0 {
    val expectedIncoming = Array[Int](0, 1)
    val m = PageRank.adjMatrix(location, nPages)
    val incoming0 = PageRank.incoming(0, m)
    assertArrayEquals(expectedIncoming, incoming0.toArray)
  }

  @Test
  def incomingTest1 {
    val expectedIncoming = Array[Int](0, 2)
    val m = PageRank.adjMatrix(location, nPages)
    val incoming = PageRank.incoming(1, m)
    assertArrayEquals(expectedIncoming, incoming.toArray)
  }

  @Test
  def incomingTest2 {
    val expectedIncoming = Array[Int](1)
    val m = PageRank.adjMatrix(location, nPages)
    val incoming = PageRank.incoming(2, m)
    assertArrayEquals(expectedIncoming, incoming.toArray)
  }

  @Test
  def outgoingTest0 {
    val expected = Array[Int](0, 1)
    val m = PageRank.adjMatrix(location, nPages)
    val outgoing = PageRank.outgoing(0, m)
    assertArrayEquals(expected, outgoing)
  }

  @Test
  def outgoingTest1 {
    val expected = Array[Int](0, 2)
    val m = PageRank.adjMatrix(location, nPages)
    val outgoing = PageRank.outgoing(1, m)
    assertArrayEquals(expected, outgoing)
  }

  @Test
  def outgoingTest2 {
    val expected = Array[Int](1)
    val m = PageRank.adjMatrix(location, nPages)
    val outgoing = PageRank.outgoing(2, m)
    assertArrayEquals(expected, outgoing)
  }

  @Test
  def colSumTest1 {
    val expectedSum = Seq[Double](2, 2, 1)
    val m = PageRank.adjMatrix(location, nPages)
    Range(0, 2).foreach { col: Int => assertEquals(expectedSum(col), PageRank.colSum(col, m), 0.0001) }
  }

  @Test
  def colSumTest2 {
    val expectedSum = Seq[Double](2, 2, 1)
    val m = PageRank.adjMatrix(location2, nPages)
    Range(0, 2).foreach { col: Int => assertEquals(expectedSum(col), PageRank.colSum(col, m), 0.0001) }
  }

  @Test
  def allSumsTest1 {
    val expected = DenseVector[Double](2, 2, 1)
    val m = PageRank.adjMatrix(location, nPages)
    val sums = PageRank.colSums(m)
    assertArrayEquals(expected.data, sums.values.iterator.toArray, 0.001)
  }

  @Test
  def inDegreeTest {
    val expected = List[Int](2, 2, 1)
    val m = PageRank.adjMatrix(location, nPages)
    for (i <- Range(0, 2)) {
      val in = PageRank.inDegree(i, m)
      assertEquals(expected(i), in)
    }
  }
}