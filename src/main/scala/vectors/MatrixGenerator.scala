package vectors

import io.EasyIO
import wierzba.DictionaryCLP

import scala.collection.immutable.TreeMap

object MatrixGenerator {

  def main(args: Array[String]): Unit = {
    EasyIO.executeAndDisplayElapsedTime({
      val documents = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
      val matrix = createTFIDFMatrix(new DictionaryCLP(VectorsFiles.clpLibDir), documents)
      saveMatrixToFile(VectorsFiles.matrixTFIDF, matrix)
    }, "parsing PAP, creating tf-idf matrix and saving it to file")
  }

  import collection.JavaConverters._

  implicit def javaListOfStringToScala(l: java.util.List[String]): List[String] = l.asScala.toList

  implicit def javaListOfIntsToScala(l: java.util.List[Integer]): List[Int] = l.asScala.map(_.intValue()).toList

  def createTFIDFMatrix(dictionaryCLP: DictionaryCLP, documents: Seq[String]): TreeMap[(Int, String), Double] = {
    val documentsOfTerms = documentsToDocumentsOfTerms(dictionaryCLP, documents)
    val terms = documentsToTerms(dictionaryCLP, documents)
    val documentFrequency = calculateDocumentFrequency(documentsOfTerms.map(_.toSet), terms)
    val numOfDocuments = documents.length

    documentsOfTerms.indices.foldLeft(TreeMap.empty[(Int, String), Double]) { (acc, i) =>
      val wordOccurencesInDocument = countWordOccurences(documentsOfTerms(i))
      val newMap = wordOccurencesInDocument.map { case (term, occurences) =>
        (i, term) -> TFIDFMetric(occurences, numOfDocuments, documentFrequency(term))
      }
      acc ++ newMap
    }
  }

  def readDocumentsWithKeywordsFromFile(fileName: String): Map[Int, Map[String, Double]] = {
    matrixToKeywords(readMatrixFromFile(fileName))
  }

  def matrixToKeywords(matrix: Seq[(Int, String, Double)], numOfKeywords: Int = 10): Map[Int, Map[String, Double]] = {
    val documents = matrix.groupBy(_._1)
    documents.mapValues {
      _.map(doc => (doc._2, doc._3))
        .toMap
    }
  }

  def saveMatrixToFile(fileName: String, matrix: TreeMap[(Int, String), Double]) = {
    def printingFun(seq: ((Int, String), Double)): String = seq match {
      case ((docId, term), metric) => s"$docId, $term, $metric"
    }
    EasyIO.saveToFileWithPrefix(fileName, matrix.toSeq, printingFun)
  }

  def readMatrixFromFile(fileName: String): Seq[(Int, String, Double)] = {
    def readingFun(line: String): (Int, String, Double) = {
      line.split(", ").toList match {
        case docId :: term :: metric :: Nil => (docId.toInt, term, metric.toDouble)
      }
    }
    EasyIO.readFileWithPrefix(fileName, readingFun)
  }

  def TFIDFMetric(tf: Int, N: Int, df: Int): Double =
    tf * Math.log(N.toDouble / df)

  def countWordOccurences(termsInDocument: Array[String]): Map[String, Int] = {
    termsInDocument.foldLeft(Map.empty[String, Int]) { (acc, word) =>
      acc + (word -> (acc.getOrElse(word, 0) + 1))
    }
  }

  def calculateDocumentFrequency(documentsOfTerms: Seq[Set[String]], terms: Set[String]): Map[String, Int] = {
    terms.foldLeft(Map.empty[String, Int]) { (acc, term) =>
      val termCount = documentsOfTerms.count(_.contains(term))
      acc + (term -> termCount)
    }
  }

  def documentsToDocumentsOfTerms(dictionaryCLP: DictionaryCLP, documents: Seq[String]): Seq[Array[String]] = {
    documents.map { document =>
      val words = document.split(" ")
      words.flatMap(termsFromWord(dictionaryCLP, _))
    }
  }

  def documentsToTerms(dictionaryCLP: DictionaryCLP, documents: Seq[String]): Set[String] = {
    val terms = for {
      document <- documents
      word <- document.split(" ")
      term <- termsFromWord(dictionaryCLP, word)
    } yield term
    terms.toSet
  }

  def termsFromWord(dictionaryCLP: DictionaryCLP, word: String): List[String] = {
    for {
      id <- dictionaryCLP.clp_rec(word)
      baseForm <- dictionaryCLP.clp_bform(id)
    } yield baseForm
  }

}
