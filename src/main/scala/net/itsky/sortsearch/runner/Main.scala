package net.itsky.sortsearch.runner

import net.itsky.sortsearch.io.FileIO


object Main {
  def main(args: Array[String]): Unit = {
    val fileName = args(0)
    val numberOfLines : Int = Integer.valueOf(args(1))
    // n=native
    // h=heapsort
    // f=flashsort
    // p=parallelflashsort
    val algorithm = args(2).toLowerCase()
    val metricName : String = if (algorithm == "f" || algorithm =="p") {
      args(3)
    } else {
      ""
    }
    val nSegments = if (algorithm == "p") {
      Integer.valueOf(args(4))
    } else {
      1
    }
    val unsorted = read(fileName, numberOfLines)

  }

  def read(fileName : String, numberOfLines : Int) : Seq[String] = {
    val t0 = System.currentTimeMillis()
    val lines = FileIO.readLines(fileName, numberOfLines)
    val t1 = System.currentTimeMillis()
    val lineCount = lines.size
    val charCount = lines.map(l => l.length.longValue()).sum
    println("file=" + fileName + " numberOfLines=" + lineCount + "(" + numberOfLines + ") " + charCount + " chars")
    lines
  }

  //def sort(lines : Seq[String], algorithm : String, metric : String)
}
