import java.nio.charset.{CodingErrorAction, StandardCharsets}

import scala.io.{Codec, Source, StdIn}

object LanguageGuesser {
  def main(args: Array[String]) {
    val codec = Codec.ISO8859.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    val languages = Seq("english", "finnish", "german", "italian", "polish", "spanish")
    val statsDir = "resources/stats/"

    println("Enter n: ")
    val n = StdIn.readInt()
    println("Enter sentence: ")
    val input = StdIn.readLine().split("\\W+")

    val inputStats = StatsBuilder.createStatistics(n, input)

    def similarity(language: String): Double = {
      val languageStats = fileToStats(s"$statsDir$language$n", codec)
      calculateSimilarity(inputStats, languageStats)
    }

    //    // full stats:
        languages.foreach(language => {
          printf("%s -> %.3f\n", language, similarity(language))
        })

    // guessed language:
    languages
      .map(lang => (lang, similarity(lang)))
      .minBy(_._2)
    match {
      case (lang, similarity) => printf("This text is written in %s (score %.3f)", lang, similarity)
    }
  }

  def calculateAllGuesses(givenLanguage: String, n: Int, input: Seq[String]): Unit = {
    val codec = Codec.ISO8859.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    val languages = Seq("english", "finnish", "german", "italian", "polish", "spanish")
    val statsDir = "resources/stats/"
    val guessesDir = "resources/guesses/"

    val inputStats = StatsBuilder.createStatistics(n, input)

    def similarity(language: String): Double = {
      val languageStats = fileToStats(s"$statsDir$language$n", codec)
      calculateSimilarity(inputStats, languageStats)
    }

    import java.io._
    val fileName = s"$guessesDir$givenLanguage$n"
    val p = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.ISO_8859_1), true)
    try {
      languages.foreach(language => {
        val sim = similarity(language)
        p.println(s"$language -> $sim")
        //        p.printf("%s -> %.3f\n", language, similarity(language))
      })
    } finally {
      p.close()
    }
  }

  def calculateSimilarity(stats1: Map[String, Int], stats2: Map[String, Int]): Double = {
    val sum = stats1.foldLeft(0) {
      case (currSum, (nGram, count)) => currSum + stats2.getOrElse(nGram, 0) * count
    }

    def len(stats: Map[String, Int]): Double = math.sqrt(stats.values.foldLeft(0: Double)((acc, a) => acc + math.pow(a, 2)))
    1 - sum / (len(stats1) * len(stats2))
  }

  def fileToStats(fileName: String, codec: Codec): Map[String, Int] = {
    val lines = Source.fromFile(fileName)(codec).getLines()
    lines.foldLeft(Map.empty[String, Int]) {
      (currMap, line) => currMap + (strToTuple(line)._1 -> strToTuple(line)._2)
    }
  }

  def strToTuple(str: String): (String, Int) = str.split(":") match {
    case Array(nGram, count) => (nGram, count.toInt)
  }

}
