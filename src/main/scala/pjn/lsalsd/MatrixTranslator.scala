package pjn.lsalsd

import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.distributed.RowMatrix
import org.apache.spark.mllib.linalg.{Matrix, SingularValueDecomposition, Vector, Vectors}
import org.apache.spark.rdd.RDD

import scala.annotation.tailrec

class MatrixTranslator(private val rawMatrix: Seq[(Int, String, Double)]) {
  val wordToIndex = seqToWordToIndex(rawMatrix)
  val indexToWord = wordToIndex.map(_.swap)
  val sparkVector = matrixToVectors(rawMatrix)

  def getRDDVectors(sc: SparkContext): RDD[Vector] = {
    sc.parallelize(sparkVector)
  }

  def getRDDVectorsWithIndices(sc: SparkContext): RDD[(Long, Vector)] = {
    val vectorsWithIndices = sparkVector.indices.map(i => (i.toLong, sparkVector(i)))
    sc.parallelize(vectorsWithIndices)
  }

  def matrixToVectors(seq: Seq[(Int, String, Double)]): Seq[Vector] = {
    def groupByDocId(seq: Seq[(Int, String, Double)]): Seq[Seq[(Int, Double)]] = {
      val groupedByDocId = seq.groupBy { case (docId, word, value) => docId }
      val sortedGroups = groupedByDocId.toSeq.sortBy(_._1)
      val docToAllWords = sortedGroups.map { case (id, entries) =>
        entries.map { case (docId, word, value) => (wordToIndex(word), value) }
      }
      docToAllWords
    }
    groupByDocId(seq).map(row => Vectors.sparse(wordToIndex.size, row))
  }

  def topTopics(m: Matrix, howMany: Int): Seq[Array[(String, Double)]] = {
    def matrixToVector(m: Matrix): Seq[Array[Double]] = m.transpose.toArray.grouped(m.numRows).toVector
    matrixToVector(m).map { termsRow =>
      termsRow.zipWithIndex.sortBy(-_._1).take(howMany)
        .map(pair => (indexToWord(pair._2), pair._1))
    }
  }

  def topTermsPerTopics(svd: SingularValueDecomposition[RowMatrix, Matrix], howMany: Int): Seq[Array[(String, Double)]] = {
    val terms = svd.V.transpose
    val twoDimentionalTerms = terms.toArray.grouped(terms.numCols).toVector
    twoDimentionalTerms.map { termsVector =>
      termsVector
        .zipWithIndex
        .sortBy(-_._1)
        .take(howMany)
        .map(pair => (indexToWord(pair._2), pair._1))
    }
  }

  private def seqToWordToIndex(seq: Seq[(Int, String, Double)]): Map[String, Int] = {
    @tailrec
    def traverse(seq: Seq[(Int, String, Double)], currMap: Map[String, Int], indexCounter: Int): Map[String, Int] = {
      if (seq.isEmpty) currMap
      else seq.head match {
        case (docId, word, value) =>
          if (currMap.contains(word)) traverse(seq.tail, currMap, indexCounter)
          else traverse(seq.tail, currMap + (word -> indexCounter), indexCounter + 1)
      }
    }
    traverse(seq, Map.empty, 0)
  }
}