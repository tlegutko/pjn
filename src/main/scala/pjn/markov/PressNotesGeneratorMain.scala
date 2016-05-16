package pjn.markov

import pjn.ngrams.SentenceNGram
import pjn.io.EasyIO

object PressNotesGeneratorMain {
  val sourceFile = "src/main/resources/pjn.markov/pap.txt"

  def main(args: Array[String]) = {
    val texts = EasyIO.readLinesFromUTF8File(sourceFile).mkString(" ").split("#\\d*").map(_.replaceAll("\\s+", " "))
    val markovChain = new MarkovChain(SentenceNGram.splitToNGrams(3, texts))
    for (i <- 1 to 30) {
      println(markovChain.generatePressNote(60))
    }
  }

}
