package vectors

import io.EasyIO

import scala.io.StdIn

/** Requires generated matrix from MatrixGenerator. **/
object DocumentByWordsFinder {
  def main(args: Array[String]) {
    val documentsWithKeywords = MatrixGenerator.readDocumentsWithKeywordsFromFile(VectorsFiles.matrixTFIDF)
    val papText = EasyIO.readPAPNotes(VectorsFiles.pap)
    while (true) {
      print("Insert csv of keywords in base form: ")
      val keywords = StdIn.readLine.split(", ")
      val mostSimilarDocuments = findMostSimilarDocs(documentsWithKeywords, keywords, 5)
      prettyPrint(mostSimilarDocuments, papText)
    }
  }

  def findMostSimilarDocs(documentsWithKeywords: Map[Int, Map[String, Double]], keywords: Seq[String], howMany: Int): Seq[(Int, Double)] = {
    val seq = documentsWithKeywords.toSeq.map { case (docId, terms) =>
      val similarityToKeywords = keywords.map(terms.getOrElse(_, 0.0)).sum
      (docId, similarityToKeywords)
    }
    seq.sortBy(-_._2)
      .take(howMany)
  }

  def prettyPrint(bestDocs: Seq[(Int, Double)], text: Seq[String]): Unit = {
    bestDocs.foreach { case (docId, metric) =>
      println(f"#$docId%06d ($metric%.3f)")
      println(text(docId))
      println("#########################")
    }
  }

}
