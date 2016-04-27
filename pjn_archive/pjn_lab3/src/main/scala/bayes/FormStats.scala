package bayes

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}

import com.google.common.io.Files

import scala.collection.JavaConversions._
import scala.io.Source

object FormStats {

  val pathPrefix = "src/main/resources/"
  val inputFiles = Seq("proza.iso.utf8", "dramat.iso.utf8", "publ.iso.utf8", "popul.iso.utf8", "wp.iso.utf8")
    .map(pathPrefix + "input/" + _)
  val formFile = pathPrefix + "input/formy.txt"
  val outputFile = pathPrefix + "output/formsStats.txt"

  def main(args: Array[String]) = {
    val forms = Files.readLines(new File(formFile), Charset.forName("ISO-8859-2")).toList
    val inputTexts = inputFiles.flatMap(Source.fromFile(_).getLines().toList).toList
    saveStatsToFile(outputFile, calculateFormStats(forms, inputTexts))
  }

  def calculateFormStats(forms: List[String], allInputLines: List[String]): Map[String, Double] = {
    val allWords = allInputLines.flatMap(_.split("[\\s\\.\\,]").filter(word => !word.isEmpty && word.matches("[^0-9\\.\\,]+")))
    val textsStats = allWords.foldLeft(Map.empty[String, Int]) {
      (wordCount, word) => wordCount + (word -> (wordCount.getOrElse(word, 0) + 1))
    }
    val formsCount = forms.foldLeft(Map.empty[String, Int]) {
      (m, form) => m + (form -> textsStats.getOrElse(form, 0))
    }
    val numOfElems = allWords.size.toDouble
    println(numOfElems)
    formsCount.mapValues(_.toDouble)
  }

  def saveStatsToFile(fileName: String, stats: Map[String, Double]): Unit = {
    import java.io._
    val p = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8), true)
    try {
      stats.toSeq.sortBy(_._2).foreach(s => p.println(s._1 + ":" + s._2))
    } finally {
      p.close()
    }
  }
}
