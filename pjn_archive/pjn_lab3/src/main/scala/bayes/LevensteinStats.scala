package bayes

import scala.collection.immutable.TreeMap
import scala.io.Source

object LevensteinStats {

  val pathPrefix = "src/main/resources/"
  val inputFile = pathPrefix + "input/bledy.txt"
  val outputFile = pathPrefix + "output/levensteinStats.txt"

  def main(args: Array[String]) {
    val lines = Source.fromFile(inputFile).getLines().toList
    saveStatsToFile(outputFile, calculateLevensteinStats(lines))
  }

  def calculateLevensteinStats(lines: List[String]): Map[Double, Double] = {
    val levensteinCounts = lines.foldLeft(TreeMap.empty[Double, Int]) {
      (levCount, line) => {
        val splitLine = line.split(";")
        val levensteinLength = LevensteinLength(splitLine.head, splitLine.last)
        levCount + (levensteinLength -> (levCount.getOrElse(levensteinLength, 0) + 1))
      }
    }
    levensteinCounts.mapValues(_ / lines.length.toDouble)
  }

  def saveStatsToFile(fileName: String, stats: Map[Double, Double]): Unit = {
    import java.io._
    //    val p = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8), true)
    val p = new PrintWriter(new File(fileName))
    try {
      stats.foreach(s => p.println(s._1 + ":" + s._2))
    } finally {
      p.close()
    }
  }

}
