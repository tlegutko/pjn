package pjn.lsalsd

import org.apache.spark.mllib.clustering.{DistributedLDAModel, LDA}
import pjn.io.EasyIO
import pjn.spark.MySpark
import pjn.vectors.{MatrixGenerator, VectorsFiles}

import scala.annotation.tailrec
import scala.io.StdIn

object SparkLDAGenerator {

  val NumOfTopics = 50

  def main(args: Array[String]) {
    val sc = MySpark.createSparkContext()

    val matrixFromFile = MatrixGenerator.readMatrixFromFile(VectorsFiles.termDocMatrix)
    val matrixTranslator = new MatrixTranslator(matrixFromFile)

    val lda = EasyIO.executeAndDisplayElapsedTime(
      new LDA().setK(NumOfTopics).run(matrixTranslator.getRDDVectorsWithIndices(sc)),
      "calculating lda in spark")
    lda.save(sc, LsaLdaFiles.sparkLDAModel)
  }
}

object SparkTopTopicsLDA {
  def main(args: Array[String]) {
    val sc = MySpark.createSparkContext()
    val lda = DistributedLDAModel.load(sc, LsaLdaFiles.sparkLDAModel)
    val matrixFromFile = MatrixGenerator.readMatrixFromFile(VectorsFiles.termDocMatrix)
    val matrixTranslator = new MatrixTranslator(matrixFromFile)

    val topTopics = matrixTranslator.topTopics(lda.topicsMatrix, 10)
    SparkLSAGenerator.saveToFile(LsaLdaFiles.ldaTopTopicsFile, topTopics)
  }
}

object SparkTopicsPerDocLDA {
  def main(args: Array[String]) {
    val sc = MySpark.createSparkContext()
    val lda = DistributedLDAModel.load(sc, LsaLdaFiles.sparkLDAModel)
    val matrixFromFile = MatrixGenerator.readMatrixFromFile(VectorsFiles.termDocMatrix)
    val matrixTranslator = new MatrixTranslator(matrixFromFile)
    val papText = EasyIO.readPAPNotes(VectorsFiles.pap)

    val topTopics = matrixTranslator.topTopics(lda.topicsMatrix, 10)
    val t = lda.topTopicsPerDocument(3).collect
    val topDocsPerTerm = lda.topTopicsPerDocument(3).map { case (id, indices, values) =>
      (id + 1, papText(id.toInt + 1), indices.map(topTopics(_)), values)
    }.collect.sortBy(_._1)
    saveToFile(LsaLdaFiles.topicsPerDocLDA, topDocsPerTerm)
  }

  def saveToFile(file: String, seq: Seq[(Long, String, Array[Array[(String, Double)]], Array[Double])]): Unit = {
    def projection(seq: (Long, String, Array[Array[(String, Double)]], Array[Double])): String = seq match {
      case (ind, text, metric, vals) =>
        val index = f"#$ind%06d"
        val topics = metric.zip(vals).map { case (termArr, value) =>
          s"$value ${termArr.mkString(", ")}"
        }.mkString("\n")
        s"$index\n$topics\n$text"
    }
    EasyIO.saveToFileWithPrefix(file, seq, projection)
  }

}

object SparkSimilarTopicFinder {
  def main(args: Array[String]) {
    val sc = MySpark.createSparkContext()
    val lda = DistributedLDAModel.load(sc, LsaLdaFiles.sparkLDAModel)
    val matrixFromFile = MatrixGenerator.readMatrixFromFile(VectorsFiles.termDocMatrix)
    val matrixTranslator = new MatrixTranslator(matrixFromFile)
    val papText = EasyIO.readPAPNotes(VectorsFiles.pap)
    while (true) {
      print("Enter PAP document number: ")
      val docId = StdIn.readInt() - 1

      val topTopicsPerDoc = lda.topTopicsPerDocument(lda.k).filter(_._1.toInt == docId).first
      val topTopicsAsPair = topTopicsPerDoc._2.zip(topTopicsPerDoc._3)
      val topDocsPerTopic = lda.topDocumentsPerTopic(5000)

      val similarDocs = findSimilarDocs(topTopicsAsPair, topDocsPerTopic, papText.size, 5)
      prettyPrint(similarDocs, papText, docId)
    }
  }

  def findSimilarDocs(seq: Array[(Int, Double)], docsPerTopics: Array[(Array[Long], Array[Double])], numOfDocs: Int, howMany: Int): Seq[(Int, Double)] = {
    @tailrec
    def traverse(seq: Array[(Int, Double)], result: scala.collection.mutable.Map[Int, Double]): Seq[(Int, Double)] = {
      if (seq.isEmpty) result.toSeq.sortBy(-_._2).take(howMany)
      else {
        val (topicId, topicWeight) = seq.head
        val dpt = docsPerTopics(topicId)
        val docsPerT = dpt._1.zip(dpt._2).map { case (docId, docWeigth) =>
          (docId.toInt, docWeigth * topicWeight)
        }
        val newResult = docsPerT.foldLeft(result) { case (res, (index, value)) =>
          res += (index -> (res.getOrElse(index, 0.0) + value))
        }
        traverse(seq.tail, newResult)
      }
    }
    traverse(seq, scala.collection.mutable.Map.empty)
  }

  def prettyPrint(bestDocs: Seq[(Int, Double)], text: Seq[String], originalDocId: Int): Unit = {
    println(text(originalDocId+1))
    bestDocs.foreach { case (docId, metric) =>
      println(f"#${docId+1}%06d ($metric%.3f)")
      println(text(docId+1))
      println("#########################")
    }
  }

}
