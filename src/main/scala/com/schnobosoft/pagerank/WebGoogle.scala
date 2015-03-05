package com.schnobosoft.pagerank

/**
 * Run PageRank on data from {@link https://snap.stanford.edu/data/web-Google.html}.
 */
object WebGoogle extends App {
  //  val nLines = Int.MaxValue // number of lines to read (in file: 5105043)
  val nPages = 916428
  val location = "/home/schnober/corpora/web-Google.txt"

  val r = PageRank.pagerank(location, nPages, method = PageRank.Method.MATRIX)

  println(r.slice(0, 10))
}