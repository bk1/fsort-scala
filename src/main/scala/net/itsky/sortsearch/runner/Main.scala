package net.itsky.sortsearch.runner

import net.itsky.sortsearch.fsort.{FlashSort, HeapSort, ParallelSort, StandardMetrics}
import net.itsky.sortsearch.io.FileIO

import scala.collection.mutable
import scala.concurrent.ExecutionContext


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
    val metric = metricName match {
      case "utf16" => StandardMetrics.utf16
      case "cyr" => StandardMetrics.cyr
      case "iso646irv" => StandardMetrics.iso646irv
      case "latin1" => StandardMetrics.iso88591
      case "iso88591" => StandardMetrics.iso88591
      case _ => StandardMetrics.utf16
    }
    val nSegments : Int = if (algorithm == "p") {
      Integer.valueOf(args(4))
    } else {
      1
    }
    val unsorted = read(fileName, numberOfLines)
    val sorted = sort(unsorted, algorithm, metric, nSegments)
    check(sorted)
  }

  def sort(lines : Seq[String], algorithm : String, metric : String => Long, nSegments : Int) : Seq[String] = {
    val t0 = System.currentTimeMillis()
    try {
      algorithm match {
        case "n" => lines.sorted(Ordering.String)
        case "h" => {
          val sortable = lines.to[mutable.IndexedSeq]
          HeapSort.hsort(sortable, Ordering.String)
          sortable
        }
        case "f" => {
          FlashSort.fsort(lines.to[mutable.IndexedSeq], Ordering.String, metric)
        }
        case "p" => {
          val ec = ExecutionContext.global
          ParallelSort.fsortParallel(lines.toIndexedSeq, Ordering.String, metric, nSegments, ec)
        }
        case _ =>
          throw new IllegalArgumentException("algorithm " + algorithm + " not supported")
      }
    } finally {
      val t1 = System.currentTimeMillis()
      println("t=" + (t1 -t0) + "msec for sorting. algorithm=" + algorithm)

    }
  }

  def check(lines : Seq[String]) : Unit = {
    var pos = 0
    lines.foldLeft(lines(0))((x,y)=> {
      if (x > y) {
        println("pos=" + pos + " x=\"" + x + "\" y=\"" + y + "\" out of order")
      }
      pos+=1
      y
    })
  }

  def write(fileName : String, sorted : Seq[String]) : Unit = {

  }

  def read(fileName : String, numberOfLines : Int) : Seq[String] = {
    val t0 = System.currentTimeMillis()
    val lines = FileIO.readLines(fileName, numberOfLines)
    val t1 = System.currentTimeMillis()
    val lineCount = lines.size
    val charCount = lines.map(l => l.length.longValue()).sum
    println("t=" + (t1 -t0) + "msec file=" + fileName + " numberOfLines=" + lineCount + "(" + numberOfLines + ") " + charCount + " chars " + (charCount / lineCount.doubleValue()) + " chars per line")
    lines
  }

  //def sort(lines : Seq[String], algorithm : String, metric : String)
}
