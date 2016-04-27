package clusters

import scala.io.Source
import scala.util.Random

object ClusterComparator {

  val inputPath = "src/main/resources/lines.txt"
  val referenceClusterPath = "src/main/resources/clusters.txt"
  val myClusterPath = "src/main/resources/myClusters.txt"
  val lineBreak = "##########"

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(inputPath).getLines().toList
    val referenceCluster = fileToClusters(referenceClusterPath)
    val myCluster = fileToClusters(myClusterPath)

//    val selectedInput = Random.shuffle(input).take(200)
    val selectedInputPairs = for {
      input1 <- Random.shuffle(input).take(300)
      input2 <- Random.shuffle(input).take(300)
    } yield (input1, input2)
//    selectedInputPairs.foreach(println)
    val (precision, recall, f1) = calculateCorrectness(selectedInputPairs, myCluster, referenceCluster)
    println(s"precision: $precision\nrecall: $recall\nF1:$f1")

  }

  def calculateCorrectness(inputPairs: List[(String, String)],
                           myCluster: Set[Set[String]], referenceCluster: Set[Set[String]]): (Double, Double, Double) = {
    val (true_pos, false_pos, false_neg) = inputPairs.foldLeft((0, 0, 0))((current, pair) => {
      current match {
        case (true_pos, false_pos, false_neg) => {
          def isPairInCluster(cluster: Set[Set[String]]): Boolean = {
            cluster.exists(s => s.contains(pair._1) && s.contains(pair._2))
          }
          val isInMyCluster = isPairInCluster(myCluster)
          val isInReferenceCluster = isPairInCluster(referenceCluster)

          if (isInMyCluster && isInReferenceCluster) (true_pos + 1, false_pos, false_neg)
          else if (isInMyCluster && !isInReferenceCluster) (true_pos, false_pos + 1, false_neg)
          else if (!isInMyCluster && isInReferenceCluster) (true_pos, false_pos, false_neg + 1)
          else (true_pos, false_pos, false_neg)
        }
      }
    })
    println(s"$true_pos, $false_pos, $false_neg")
    val precision = true_pos.toDouble / (true_pos + false_pos)
    val recall = true_pos.toDouble / (true_pos + false_neg)
    val f1 = 2 * (precision * recall) / (precision + recall)
    (precision, recall, f1)
  }

  def fileToClusters(fileName: String): Set[Set[String]] = {
    Source.fromFile(fileName).getLines().filter(_.nonEmpty)
      .foldLeft((Set.empty[Set[String]], Set.empty[String])) {
        (currSets, line) => {
          if (line == lineBreak) (currSets._1 + currSets._2, Set.empty)
          else (currSets._1, currSets._2 + line)
        }
      }._1 // only bigger set
  }
}
