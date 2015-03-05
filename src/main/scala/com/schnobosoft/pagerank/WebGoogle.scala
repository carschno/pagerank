package com.schnobosoft.pagerank

//import com.schnobosoft.pagerank.PageRank.pagerank

object WebGoogle extends App {
  val nLines = Int.MaxValue // number of lines to read (in file: 5105043)
  val nPages = 916427
  val location = "/home/schnober/corpora/web-Google.txt"

  val r = PageRank.pagerank(location, nPages, nLines, PageRank.Method.MATRIX)

  println(r.slice(0, 10))
}