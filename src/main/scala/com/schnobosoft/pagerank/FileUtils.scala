package com.schnobosoft.pagerank
import scala.io.Source.fromFile

/**
 * @author Carsten Schnober
 */
object FileUtils {
  private final val COMMENT_MARKER = "#"
  private final val COLUMN_SEPARATOR = "\t"

  /**
   * Get an iterator over the lines of a file. Each line is expected to contain a pair of two integers,
   * separated by a TAB. Comments are ignored. The iterator thus returns tuples of integers.
   *
   * @param location  the location of the input file
   * @param nLines  if given, read this number of lines only
   * @return an iterator over tuples of two integers
   */
  def fileIterator(location: String, nLines: Int = Int.MaxValue): Iterator[(Int, Int)] = {
    iteratorHead(fromFile(location).getLines
      .filter { !_.startsWith(COMMENT_MARKER) }, nLines) // ignore comments
      .map { (_.split(COLUMN_SEPARATOR)) } // split line
      .map { (x => (Integer.parseInt(x(0)), Integer.parseInt(x(1)))) } // convert to Integers
  }

  /**
   * Reduce an iterator of definite size to its n leading entries. If the iterator does have a
   * definite size and its size is smaller than the given n, return it unchanged.
   *
   * @param iter an iterator
   * @param n the maximum number of entries to return for the iterator
   * @return an iterator with the given number of entries max.
   */
  def iteratorHead[T](iter: Iterator[T], n: Int): Iterator[T] = {
    if (iter.hasDefiniteSize && iter.size < n)
      iter
    else
      iter.take(n)
  }

}