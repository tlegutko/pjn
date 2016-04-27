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
  def readLinesFromISO8859File(fileName: String): List[String] = {
    import scala.collection.JavaConversions._
    Files.readLines(new File(fileName), Charset.forName("ISO-8859-2")).toList
  }
}
