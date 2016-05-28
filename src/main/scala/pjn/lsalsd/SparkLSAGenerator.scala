package pjn.lsalsd

import org.apache.spark.mllib.linalg.distributed.RowMatrix
import org.apache.spark.{SparkConf, SparkContext}
import pjn.io.EasyIO
import pjn.spark.MySpark
import pjn.vectors.{MatrixGenerator, VectorsFiles}

object SparkLSAGenerator {
  val NumOfTopics = 50

  def main(args: Array[String]) {
    val sc = new SparkContext(new SparkConf().setAppName("SparkLSAGenerator"))

    val matrixFromFile = MatrixGenerator.readMatrixFromFile(VectorsFiles.matrixTFIDF)
    val matrixTranslator = new MatrixTranslator(matrixFromFile)
    val matrix = new RowMatrix(matrixTranslator.getRDDVectors(sc))

    val svd = EasyIO.executeAndDisplayElapsedTime(
      matrix.computeSVD(NumOfTopics, computeU = true),
      "calculating SVD")
    val topTerms = matrixTranslator.topTermsPerTopics(svd, 10)
    saveToFile(LsaLdaFiles.lsaFile + 2, topTerms)
  }

  def saveToFile(fileName: String, seq: Seq[Array[(String, Double)]]): Unit = {
    def projection(line: Array[(String, Double)]): String = line.mkString("; ")
    EasyIO.saveToFileWithPrefix(fileName, seq, projection)
  }

}

object SparkTopicsPerDoc {
  val NumOfTopics = 300

  def main(args: Array[String]) {
    val sc = MySpark.createSparkContext()

    val matrixFromFile = MatrixGenerator.readMatrixFromFile(VectorsFiles.matrixTFIDF)
    val matrixTranslator = new MatrixTranslator(matrixFromFile)
    val matrix = new RowMatrix(matrixTranslator.getRDDVectors(sc))

    val papText = EasyIO.readPAPNotes(VectorsFiles.pap)

    val svd = EasyIO.executeAndDisplayElapsedTime(
      matrix.computeSVD(NumOfTopics, computeU = true),
      "calculating SVD")

    val topTerms = matrixTranslator.topTermsPerTopics(svd, 10)

    val docsWithTopics = svd.U.rows.zipWithIndex().map{ case (vector, id) =>
      val topicsPerDoc = vector.toArray.zipWithIndex.sortBy(-_._1).take(3).map {
        case (value, index) => (value, topTerms(index))
      }
      (id.toInt+1, papText(id.toInt+1), topicsPerDoc)
    }.collect().sortBy(_._1)

    saveToFile(LsaLdaFiles.topicsPerDocLSA, docsWithTopics)

  }

  def saveToFile(file: String, seq: Array[(Int, String, Array[(Double, Array[(String, Double)])])]): Unit = {
    def projection(s: (Int, String, Array[(Double, Array[(String, Double)])])): String = {
      s match { case (ind, text, metric) =>
        val index = f"#$ind%06d"
        val topics = metric.map { case (value, termArr) =>
          s"$value ${termArr.mkString(", ")}"
        }.mkString("\n")
        s"$index\n$topics\n$text"
      }
    }
    EasyIO.saveToFileWithPrefix(file, seq, projection)
  }

}