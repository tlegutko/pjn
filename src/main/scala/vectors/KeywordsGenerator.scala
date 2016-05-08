package vectors

import io.EasyIO

/** Requires generated matrix from MatrixGenerator. **/
object KeywordsGenerator {
  def main(args: Array[String]) {
    EasyIO.executeAndDisplayElapsedTime({
      val documentsWithKeywords = MatrixGenerator.readDocumentsWithKeywordsFromFile(VectorsFiles.matrixTFIDF)
      val bestKeywords = documentsToBestKeywords(documentsWithKeywords, 8)
      saveKeywordsToFile(VectorsFiles.keywords, bestKeywords)
    }, "generating keywords")
  }

  def documentsToBestKeywords(documents: Map[Int, Map[String, Double]], numOfKeywords: Int): Seq[(Int, Seq[(String, Double)])] = {
    val sortedDocs = documents.toSeq.sortBy(_._1)
    sortedDocs.map { case (docId, doc) =>
      val bestKeywords = doc.toSeq.sortBy(-_._2).take(numOfKeywords)
      (docId, bestKeywords)
    }
  }

  def saveKeywordsToFile(fileName: String, keywords: Seq[(Int, Seq[(String, Double)])]) = {
    def lineProjection(line: (Int, Seq[(String, Double)])): String = line match {
      case (docId, docKeywords) =>
        val prettyKeywords = docKeywords.map{k => f"${k._1} (${k._2}%.3f)"}
        f"#$docId%06d ${prettyKeywords.mkString(", ")}"
    }
    EasyIO.saveToFileWithPrefix(fileName, keywords, lineProjection)
  }

}
