package com.schnobosoft.pagerank.breeze

/**
 * Run PageRank on data from {@link https://snap.stanford.edu/data/web-Google.html}.
 * Adapt the location of the input file as appropriate.
 */
object WebGoogle extends App {
  val outputNodes = List[Int](0, 99);
  //  val nLines = Int.MaxValue // number of lines to read (in file: 5105043)
  val nPages = 916428

  val location = System.getenv("HOME") + "/corpora/web-Google.txt"

  val r = PageRank.pagerank(location, nPages, method = PageRank.Method.ITERATIVE, beta = 0.8)

  for (node <- outputNodes)
    printf("Node %d:\t%E%n", node, r(node))
}