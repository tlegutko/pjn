package pjn.vectors

import pjn.io.EasyIO

import scala.io.StdIn

/** Requires generated matrix from MatrixGenerator. **/
object DocumentBySimilarityFinder {
  def main(args: Array[String]) {
    val documentsWithKeywords = MatrixGenerator.readDocumentsWithKeywordsFromFile(VectorsFiles.matrixTFIDF)
    val papText = EasyIO.readPAPNotes(VectorsFiles.pap)
    val keywords = EasyIO.readPAPNotes(VectorsFiles.keywords)
    while (true) {
      print("Enter PAP document number: ")
      val docId = StdIn.readInt()
      val similarDocs = findSimilarDocs(documentsWithKeywords, docId, 10)
      prettyPrint(similarDocs, papText, keywords, docId)
    }
  }

  def findSimilarDocs(docs: Map[Int, Map[String, Double]], selectedDocIndex: Int, howMany: Int): Seq[(Int, Double)] = {
    val curriedMetric = documentSimilarityMetric(docs(selectedDocIndex)) _
    docs
      .filterNot(_._1 == selectedDocIndex)
      .mapValues(curriedMetric)
      .toSeq
      .sortBy(_._2)
      .take(howMany)
  }

  def documentSimilarityMetric(doc1: Map[String, Double])(doc2: Map[String, Double]): Double = {
    def len(stats: Map[String, Double]): Double = {
      val sumOfPows = stats.values.foldLeft(0: Double)((acc, a) => acc + math.pow(a, 2))
      math.sqrt(sumOfPows)
    }
    val wordsMultiplied = doc1.foldLeft(0.0) { case (acc, (word, value)) =>
      acc + doc2.getOrElse(word, 0.0) * value
    }
    1.0 - (wordsMultiplied / (len(doc1) * len(doc2)))
  }

  def prettyPrint(bestDocs: Seq[(Int, Double)], text: Seq[String], keywords: Seq[String], originalDocId: Int): Unit = {
    println(keywords(originalDocId-1))
    println(text(originalDocId))
    bestDocs.foreach { case (docId, metric) =>
      println(f"#$docId%06d ($metric%.3f)")
      println(keywords(docId-1))
      println(text(docId))
      println("#########################")
    }
  }
}
