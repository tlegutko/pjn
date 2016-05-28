package pjn.lsalsd

import pjn.io.EasyIO
import pjn.vectors.{MatrixGenerator, VectorsFiles}
import pjn.wierzba.ScalaDictionaryCLP

object TermDocMatrixGenerator {
  def main(args: Array[String]) {
    EasyIO.executeAndDisplayElapsedTime({
      val documents = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
      val clpDict = new ScalaDictionaryCLP
      val matrix = MatrixGenerator.createTermDocumentMatrix(clpDict.javaDictionaryCLP, documents)
      MatrixGenerator.saveMatrixToFile(VectorsFiles.termDocMatrix, matrix)
    }, "creating modified term doc matrix")
  }

  def saveToFile(fileName: String, vectorRepresentation: Seq[(Int, Seq[(Int, Double)])]): Unit = {
    def projection(line: (Int, Seq[(Int, Double)])): String = line._2.mkString("; ")
    EasyIO.saveToFileWithPrefix(fileName, vectorRepresentation, projection)
  }

}
