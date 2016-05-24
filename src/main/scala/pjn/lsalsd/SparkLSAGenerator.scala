package pjn.lsalsd

import org.apache.log4j.{Level, LogManager}
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.linalg.distributed.RowMatrix
import org.apache.spark.{SparkConf, SparkContext}

object SparkLSAGenerator {

  val NumOfTopics = 10

  def main(args: Array[String]) {
    val sparkConf = new SparkConf().setAppName("SparkLSAGenerator")
    val sc = new SparkContext(sparkConf)
    LogManager.getRootLogger.setLevel(Level.WARN)
    val tdMatrixFromFile = sc.textFile(LsaLsdFiles.termDocMatrix).cache()
    val vector = tdMatrixFromFile.map { line =>
      val values = lineToSeq(line)
      Vectors.sparse(values.length, values)
    }
    val matrix = new RowMatrix(vector)
    val svd = matrix.computeSVD(NumOfTopics, computeU = true)
    val docs = svd.U
    val topics = svd.s
    val terms = svd.V
    println(docs)
  }

  def lineToSeq(line: String): Seq[(Int, Double)] = {
    def strPairToPair(s: String): (Int, Double) = {
      val regPair = """\((\d+),(\d+\.\d+)\)""".r
      s match {
        case regPair(id, value) => (id.toInt, value.toDouble)
      }
    }
    line.split("; ").map(strPairToPair(_))
  }


}