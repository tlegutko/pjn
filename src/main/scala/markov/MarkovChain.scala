package markov

import ngrams.NGram

import scala.annotation.tailrec
import scala.util.Random

class MarkovChain(nGrams: Seq[NGram]) {
  private val ngramStats = (1 until nGrams.length).foldLeft(Map.empty[NGram, List[NGram]]) {
    (currMap, i) => currMap + (nGrams(i - 1) -> (nGrams(i) :: currMap.getOrElse(nGrams(i - 1), List.empty)))
  }

  private def startsWithUpperCase(nGram: NGram): Boolean = "[A-Z]".r.pattern.matcher(nGram.str.head.toString).matches

  private val sentenceStartingNGrams = ngramStats.keys.filter(startsWithUpperCase).toVector

  def generatePressNote(length: Int): String = {
    generate(
      length,
      endCondition = (text, triesLeft) => triesLeft <= 0 && text.endsWith("."),
      startingKeyProvider = sentenceStartingNGrams(Random.nextInt(sentenceStartingNGrams.length)),
      separator = " "
    )
  }

  def generateWord(length: Int): String = {
    generate(
      length,
      endCondition = (_, triesLeft) => triesLeft == 0,
      startingKeyProvider = ngramStats.keys.toVector(Random.nextInt(ngramStats.keySet.size)),
      separator = ""
    )
  }

  private def generate(length: Int, endCondition: (String, Int) => Boolean, startingKeyProvider: NGram, separator: String): String = {
    @tailrec
    def doGenerate(key: NGram, currentChain: String, triesLeft: Int): String = {
      if (endCondition(currentChain, triesLeft)) currentChain
      else {
        val value = ngramStats.get(key).map(list => list(Random.nextInt(list.length)))
        if (value.isDefined) doGenerate(value.get, currentChain + separator + value.get.last, triesLeft - 1)
        else {
          // markov chain ended too early
          val value = startingKeyProvider
          doGenerate(value, currentChain + ". " + value, triesLeft - 1)
        }
      }
    }
    val chainStart = startingKeyProvider
    doGenerate(chainStart, chainStart.str, length)
  }
}
