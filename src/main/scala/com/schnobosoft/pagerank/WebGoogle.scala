package com.schnobosoft.pagerank

//import com.schnobosoft.pagerank.PageRank.pagerank

object WebGoogle extends App {
  val nLines = 10000 // number of lines to read (in file: 5105043)
  val nPages = 916427
  val location = "/home/schnober/corpora/web-Google.txt"

  val r = PageRank.pagerankG(location, nPages, nLines)

  println(r.slice(0, 10))
}