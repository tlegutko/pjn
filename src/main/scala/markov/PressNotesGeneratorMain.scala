package markov

import io.EasyIO
import ngrams.SentenceNGram

object PressNotesGeneratorMain {
  val sourceFile = "src/main/resources/markov/pap.txt"

  def main(args: Array[String]) = {
    val texts = EasyIO.readLinesFromUTF8File(sourceFile).mkString(" ").split("#\\d*").map(_.replaceAll("\\s+", " "))
    val markovChain = new MarkovChain(SentenceNGram.splitToNGrams(3, texts))
    for (i <- 1 to 30) {
      println(markovChain.generatePressNote(60))
    }
  }

}
