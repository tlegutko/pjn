package bayes

import java.io.File
import java.nio.charset.Charset

import com.google.common.io.Files

import scala.io.Source

object BayesSpellChecker {

  val numOfAllOccurrences = 529816
  val numOfForms = 1374254
  val pathPrefix = "src/main/resources/"
  val formStatsFile = pathPrefix + "output/formsStats.txt"
  val levensteinStatsFile = pathPrefix + "output/levensteinStats.txt"

  def main(args: Array[String]) {
    import scala.collection.JavaConversions._
    println("loading levenstein & form statistics...")
    val statsStart = System.currentTimeMillis()
    val levensteinStats = restoreLevensteinStats(Source.fromFile(levensteinStatsFile).getLines())
    val formsStats = restoreFormsStats(Files.readLines(new File(formStatsFile), Charset.forName("UTF-8")).toList)
    val timeDiff = (System.currentTimeMillis() - statsStart) / 1000.0
    println(s"loaded levenstein & form statistics in ${timeDiff}s")

    while (true) {
      println("enter word: ")
      val input = scala.io.StdIn.readLine()
      val start = System.currentTimeMillis()

      val suggestions = findSuggestions(levensteinStats, formsStats, input)
      suggestions.foreach(s => println(s"${s._1} (${s._2})"))

      val timeDiff = (System.currentTimeMillis() - start) / 1000.0
      println(s"found in ${timeDiff}s")
    }
  }

  def findSuggestions(levStats: Map[Double, Double], formsStats: Map[String, Double], input: String) = {
    formsStats
      .filter(_._2 > 0) // "optimization"
      .filter(line => line._1.length >= input.length - 1 && line._1.length <= input.length + 1)
      .keys.toList
      .map(form => (form, levensteinErrorProbability(levStats, form, input) * occurenceInLangProbability(formsStats, form)))
      .sortWith(_._2 > _._2)
      .take(5)
  }

  def occurenceInLangProbability(formsStats: Map[String, Double], s: String): Double = {
    (formsStats.getOrElse(s, 0.0) / 10000 + 1) / (numOfAllOccurrences + numOfForms)
  }

  def levensteinErrorProbability(levStats: Map[Double, Double], s1: String, s2: String): Double = {
    levStats.getOrElse(LevensteinLength(s1, s2), 0)
  }

  def restoreLevensteinStats(lines: Iterator[String]): Map[Double, Double] = {
    lines.foldLeft(Map.empty[Double, Double]) {
      (currMap, line) => {
        val splitLine = line.split(":")
        currMap + (splitLine.head.toDouble -> splitLine.last.toDouble)
      }
    }
  }

  def restoreFormsStats(lines: List[String]) = {
    lines.foldLeft(Map.empty[String, Double]) {
      (currMap, line) => {
        val splitLine = line.split(":")
        currMap + (splitLine.head -> splitLine.last.toDouble)
      }
    }
  }

}
