package pjn.lsalsd

import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.linalg.distributed.RowMatrix
import org.apache.spark.{SparkConf, SparkContext}

object SparkLSAGenerator {

  val NumOfTopics = 10

  def main(args: Array[String]) {
    val sc = new SparkContext(new SparkConf())
    val tdMatrixFromFile = sc.textFile(LsaLsdFiles.termDocMatrix).cache()
    val numOfWords = tdMatrixFromFile.groupBy { line =>
      line.split(", ") match {
        case Array(docId, term, tf) => term
      }
    }.count
    val vector = tdMatrixFromFile.map { line =>
      line.split(", ") match {
        case Array(docId, term, tf) => Vectors.dense(docId.toDouble, tf.toDouble)
      }
    }
    val matrix = new RowMatrix(vector)
    val svd = matrix.computeSVD(NumOfTopics, computeU = true)
    val docs = svd.U
    val topics = svd.s
    val terms = svd.V
    println(docs)
  }

}