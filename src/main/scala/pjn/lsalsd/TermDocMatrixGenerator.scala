package pjn.lsalsd

import pjn.io.EasyIO
import pjn.vectors.{MatrixGenerator, VectorsFiles}
import pjn.wierzba.DictionaryCLP

object TermDocMatrixGenerator {
  def main(args: Array[String]) {
    EasyIO.executeAndDisplayElapsedTime({
      val documents = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
      val matrix = MatrixGenerator.createTermDocumentMatrix(new DictionaryCLP(VectorsFiles.clpLibDir), documents)
      MatrixGenerator.saveMatrixToFile(VectorsFiles.termDocMatrix, matrix)
    }, "creating modified term doc matrix")
  }
}
