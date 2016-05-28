package pjn.io

import java.io.File
import java.nio.charset.Charset

import com.google.common.io.Files

import scala.io.Source

object EasyIO {

  val resourcesPrefix = "src/main/resources/pjn/"

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

  def readPAPNotes(fileName: String): IndexedSeq[String] = {
    readLinesFromUTF8FileWithPrefix(fileName)
      .mkString(" ")
      .split("#\\d{6}")
      .map(_.replaceAll("\\s+", " "))
  }

  def readPAPNotesDepunctuated(fileName: String): IndexedSeq[String] = {
    readLinesFromUTF8FileWithPrefix(fileName)
      .mkString(" ")
      .split("#\\d{6}")
//      .map(_.replaceAll("[\\s.,-;%*&@_#+-<>=:()!?\"\'`]+", " "))
      .map(_.replaceAll("[\\s\\d-~!@#$^%&*()_+={}\\[\\]|;:\"'`<,>.?/\\\\]+", " "))
      .map(_.toLowerCase)
  }

  def executeAndDisplayElapsedTime[T](f: => T, msg: String): T = {
    def doubleToDisplayedTime(timeDiffSec: Double): String = {
      val minutes = Math.floorDiv(timeDiffSec.toInt, 60)
      val secondFraction = timeDiffSec - timeDiffSec.floor
      val seconds = Math.floorMod(timeDiffSec.toInt, 60) + secondFraction
      f"${minutes}m$seconds%.3fs"
    }
    println(s"started $msg...")
    val statsStart = System.currentTimeMillis()
    val result = f
    val timeDiffSec = (System.currentTimeMillis() - statsStart) / 1000.0
    val executionTime = doubleToDisplayedTime(timeDiffSec)
    println(s"finished $msg in $executionTime")
    result
  }

  def saveToFileWithPrefix[T](fileName: String, elemsToSave: Seq[T], lineProjection: T => String, header: String = "") = {
    import java.io._
    val p = new PrintWriter(new File(resourcesPrefix + fileName))
    try {
      if (!header.isEmpty)
        p.println(header)
      elemsToSave.foreach(elem => p.println(lineProjection(elem)))
    } finally {
      p.close()
    }
  }

  def readFileWithPrefix[T](fileName: String, lineProjection: String => T): Seq[T] = {
    val lines = readLinesFromUTF8FileWithPrefix(fileName)
    lines.map(lineProjection)
  }

  def papNumber(id: Int): String = f"#$id%06d"

}
