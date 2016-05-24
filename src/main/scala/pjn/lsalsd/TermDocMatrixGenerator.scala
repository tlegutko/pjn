package pjn.lsalsd

import pjn.io.EasyIO
import pjn.vectors.{MatrixGenerator, VectorsFiles}
import pjn.wierzba.{DictionaryCLP, ScalaDictionaryCLP}

object TermDocMatrixGenerator {
  def main(args: Array[String]) {
    EasyIO.executeAndDisplayElapsedTime({
      val documents = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
      val clpDict = new ScalaDictionaryCLP
      val matrix = MatrixGenerator.createTermDocumentMatrix(clpDict.javaDictionaryCLP, documents)
      val vectorRepresentation = matrixToVectorRepresentation(clpDict, matrix)
      saveToFile(VectorsFiles.termDocMatrix, vectorRepresentation)
    }, "creating modified term doc matrix")
  }

  def matrixToVectorRepresentation(dict: ScalaDictionaryCLP, matrix: Map[(Int, String), Double]): Seq[(Int, Seq[(Int, Double)])] = {
    val groupedByDocId = matrix.groupBy{case ((docId, word), value) => docId}
    val docToAllWords = groupedByDocId.mapValues {
      _.flatMap { case ((docId, word), value) =>
        for {
          id <- dict.indexesOfWord(word)
        } yield (id, value)
      }.toSeq
    }
    docToAllWords.toSeq
  }

  def saveToFile(fileName: String, vectorRepresentation: Seq[(Int, Seq[(Int, Double)])]): Unit = {
    def projection(line: (Int, Seq[(Int, Double)])): String = line._2.mkString("; ")
    EasyIO.saveToFileWithPrefix(fileName, vectorRepresentation, projection)
  }

}
