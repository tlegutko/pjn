package io

import java.io.File
import java.nio.charset.Charset

import com.google.common.io.Files

import scala.io.Source

object EasyIO {

  val resourcesPrefix = "src/main/resources/"

  def readLinesFromUTF8File(fileName: String): List[String] = {
    Source.fromFile(fileName).getLines().toList
  }

  def readLinesFromISO88592File(fileName: String): List[String] = {
    import scala.collection.JavaConversions._
    Files.readLines(new File(fileName), Charset.forName("ISO-8859-2")).toList
  }

  def readLinesFromUTF8FileWithPrefix(fileName: String): List[String] = {
    readLinesFromUTF8File(resourcesPrefix + fileName)
  }

  def readLinesFromISO88592FileWithPrefix(fileName: String): List[String] = {
    readLinesFromISO88592File(resourcesPrefix + fileName)
  }

  def readPAPNotes(): Seq[String] = {
    readLinesFromUTF8FileWithPrefix("markov/pap.txt")
      .mkString(" ")
      .split("#\\d*")
      .map(_.replaceAll("\\s+", " "))
  }

  def readPAPNotesDepunctuated(): Seq[String] = {
    readLinesFromUTF8FileWithPrefix("markov/malepap.txt")
      .mkString(" ")
      .split("#\\d*")
      .map(_.replaceAll("[\\s.,-;:()!?\"\'`]+", " "))
      .filterNot(_.isEmpty)
      .map(_.toLowerCase)
  }

  def executeAndDisplayElapsedTime[T](f: => T, msg: String): T = {
    def printlnTime(timeDiffSec: Double): Unit = {
      val minutes = Math.floorDiv(timeDiffSec.toInt, 60)
      val secondFraction = timeDiffSec - timeDiffSec.floor
      val seconds = Math.floorMod(timeDiffSec.toInt, 60) + secondFraction
      println(f"$msg in ${minutes}m$seconds%.3fs")
    }
    val statsStart = System.currentTimeMillis()
    val res = f
    val timeDiffSec = (System.currentTimeMillis() - statsStart) / 1000.0
    printlnTime(timeDiffSec)
    res
  }

  def saveToFileWithPrefix[T](fileName: String, elemsToSave: Seq[T], projection: T => String) = {
    import java.io._
    val p = new PrintWriter(new File(resourcesPrefix + fileName))
    try {
      elemsToSave.foreach(elem => p.println(projection(elem)))
    } finally {
      p.close()
    }
  }

}
