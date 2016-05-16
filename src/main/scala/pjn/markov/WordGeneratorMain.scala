package pjn.markov

import pjn.ngrams.WordNGram
import pjn.io.EasyIO

import scala.util.Random

object WordGeneratorMain {
  val sourceFile = "src/main/resources/pjn.markov/srednie2pap.txt"

  def main(args: Array[String]) = {
    val texts = EasyIO
      .readLinesFromUTF8File(sourceFile)
      .mkString(" ")
      .split("#\\d*")
      .map(_.replaceAll("[\\s.,-;:()!?\"\'`]+", " "))
      .map(_.toLowerCase)
      .toVector
    for (n <- 2 to 8) {
      println(n + ":")
      val markovChain = new MarkovChain(WordNGram.splitToNGrams(n, texts))
      for (i <- 1 to 20) {
        println(markovChain.generateWord(Random.nextInt(6) + 4))
      }
    }
  }
}
