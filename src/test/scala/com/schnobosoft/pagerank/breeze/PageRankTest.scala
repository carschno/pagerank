package com.schnobosoft.pagerank.breeze

import org.junit.Assert.assertArrayEquals
import org.junit.Test

import com.schnobosoft.pagerank.breeze.PageRank

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
    var r = PageRank.pagerank(location2, 3, method = PageRank.Method.ITERATIVE)
    assertArrayEquals(expepectedR.data, r.data, PageRank.EPSILON)
  }

  @Test
  def pageRankTestNLines {
    var expepectedR = DenseVector[Double](7d / 33d, 5d / 33d, 21d / 33d)
    var r = PageRank.pagerank(location2, 3, 5, PageRank.Method.ITERATIVE)
    assertArrayEquals(expepectedR.data, r.data, PageRank.EPSILON)
  }

  @Test
  def pageRankTestMatrix {
    var expepectedR = DenseVector[Double](7d / 33d, 5d / 33d, 21d / 33d)
    var r = PageRank.pagerank(location2, 3, 5, PageRank.Method.MATRIX)
    assertArrayEquals(expepectedR.data, r.data, PageRank.EPSILON)
  }
}