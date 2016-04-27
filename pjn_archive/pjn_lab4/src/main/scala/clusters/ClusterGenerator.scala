package clusters

import scala.io.Source
import scala.util.Random

object ClusterGenerator {

  val inputPath = "src/main/resources/lines.txt"
  val referenceOutputPath = "src/main/resources/clusters.txt"
  val myOutputPath = "src/main/resources/myClusters.txt"
  val stopList = Seq("TELEPHONE:", "TEL:", "FAX:", "ROAD", "RD.", "UL.", "CO.", "-", ",", ".", " ")
  val clusterNum = 3357

  def main(args: Array[String]) {
    val parsedInput = Source.fromFile(inputPath).getLines()
      .foldLeft(Map.empty[String, String]) {
        (dict, line) => dict + (eraseStopListFromLine(line, stopList) -> line)
      }
    val clusters = clusterify(parsedInput.keys.toList, clusterNum)
    val formattedClusters = clusters.mapValues(_.map(parsedInput.getOrElse(_, "blad")))
    saveToFile(myOutputPath, formattedClusters)
  }

  def clusterify(lines: List[String], num: Int) = {
    val shuffled = Random.shuffle(lines)
    //    val clusters = shuffled.take(num).map(_.toSet)
    val clusters = (0 until num).map(x => shuffled(x) -> Set(shuffled(x))).toMap // map<str, set.(str)>
    val unassigned = shuffled.takeRight(shuffled.length - num)
    def assignToCluster(currentClusters: Map[String, Set[String]], elemToAdd: String): Map[String, Set[String]] = {
      val startingElement = (currentClusters.head._1, metricLCS(currentClusters.head._1, elemToAdd))
      val assignedCluster = currentClusters.foldLeft(startingElement) {
        (curr, cluster) => {
          val metric = metricLCS(cluster._1, elemToAdd)
          if (metric < curr._2) (cluster._1, metric) else curr
        }
      }._1 // just cluster name
      currentClusters + (assignedCluster -> (currentClusters.getOrElse(assignedCluster, Set.empty) + elemToAdd))
    }
    unassigned.foldLeft(clusters)((clusters, unassigned) => assignToCluster(clusters, unassigned))
  }

  def eraseStopListFromLine(line: String, wordsToErase: Seq[String]): String = {
    wordsToErase.foldLeft(line.toUpperCase)((currLine, wordToErase) => currLine.replaceAllLiterally(wordToErase, ""))
  }

  def metricLCS(str1: String, str2: String): Double = {
    def lcsd(a: String, b: String): Double = {
      if (a.isEmpty || b.isEmpty) 0
      else if (a == b) a.length
      else {
        val lengths = Array.ofDim[Int](a.length + 1, b.length + 1)
        for (i <- 0 until a.length)
          for (j <- 0 until b.length)
            if (a(i) == b(j))
              lengths(i + 1)(j + 1) = lengths(i)(j) + 1
            else
              lengths(i + 1)(j + 1) = scala.math.max(lengths(i + 1)(j), lengths(i)(j + 1))
        lengths(a.length)(b.length)
      }
    }
    1 - (lcsd(str1, str2) / scala.math.max(str1.length, str2.length))
  }

  def saveToFile(fileName: String, payload: Map[String, Set[String]]): Unit = {
    import java.io._
    val p = new PrintWriter(new File(fileName))
    try {
      payload.foreach(s => {
        s._2.foreach(p.println)
        p.println("##########")
      })
    } finally {
      p.close()
    }
  }

}
