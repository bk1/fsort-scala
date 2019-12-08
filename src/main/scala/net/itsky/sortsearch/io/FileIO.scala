package net.itsky.sortsearch.io

import java.io._
import java.nio.charset.{Charset, StandardCharsets}

import scala.io.Codec

object FileIO {

  val UTF8 : Charset = StandardCharsets.UTF_8
  val UTF8C : Codec = new Codec(UTF8)

  def readLines(fileName : String, maxLines : Int) : Seq[String] = {
    val bufferedSource = io.Source.fromFile(fileName)(UTF8C)
    try {
      bufferedSource.getLines.take(maxLines).toList
    } finally {
      bufferedSource.close
    }
  }


  def writeLines(fileName : String, lines : Seq[String]) : Unit = {
    val file = new File(fileName)
    val outputStream = new FileOutputStream(file)
    val fileWriter = new OutputStreamWriter(outputStream, UTF8)
    val bw = new BufferedWriter(fileWriter)
    try {
      for (line <- lines) {
        bw.write(line)
      }
    } finally {
      bw.close()
    }
  }

}
