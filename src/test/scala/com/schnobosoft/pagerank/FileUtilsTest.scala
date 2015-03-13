package com.schnobosoft.pagerank

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * @author Carsten Schnober
 */
class FileUtilsTest {

  @Test
  def iteratorMaxTest1 {
    val expected = 5
    val iterator = Iterator.from(0).take(10)
    val result = FileUtils.iteratorHead(iterator, 5)
    assertEquals(expected, result.size)
  }

  @Test
  def iteratorMaxTest2 {
    val expected = 10
    val iterator = Iterator.from(0).take(10)
    val result = FileUtils.iteratorHead(iterator, 15)
    assertEquals(expected, result.size)
  }

  @Test
  def iteratorMaxTest3 {
    val expected = 5
    val iterator = Iterator.from(0)
    val result = FileUtils.iteratorHead(iterator, 5)
    assertEquals(expected, result.size)
  }

}