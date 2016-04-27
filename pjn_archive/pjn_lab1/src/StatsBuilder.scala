import java.nio.charset.{StandardCharsets, CodingErrorAction}

import scala.io.{Codec, Source}

object StatsBuilder {
  def main(args: Array[String]) {
    val codec = Codec.ISO8859.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    val languages = Map("english" -> 4, "finnish" -> 2, "german" -> 4, "italian" -> 2, "polish" -> 3, "spanish" -> 2)

    val fileNames = generateFileNames("resources/reference/", languages, ".txt")
    val languagesWords = getWordsFromFiles(fileNames, codec)

    languagesWords.foreach({ case (language, words) => {
      for (i <- 1 to 10) {
        val stats = createStatistics(i, words)
        saveStatsToFile(s"resources/stats/$language$i", stats)
      }
    }
    })
  }

  def saveStatsToFile(fileName: String, stats: Map[String, Int]): Unit = {
    import java.io._
    val p = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.ISO_8859_1), true)
//    val p = new PrintWriter(new File(fileName))
    try {
      stats.foreach(s => p.println(s._1 + ":" + s._2))
    } finally {
      p.close()
    }
  }

  def createStatistics(n: Int, words: Seq[String]): Map[String, Int] = {

    def splitToNGrams(n: Int, words: Seq[String]): Seq[String] = {
      def wordToNGrams(word: String): Seq[String] = {
        for {
          lowerBound <- 0 to word.length - n
          upperBound = lowerBound + n
        } yield word.substring(lowerBound, upperBound)
      }
      words.flatMap(wordToNGrams)
    }

    def countNGrams(nGrams: Seq[String]): Map[String, Int] = {
      nGrams.foldLeft(Map.empty[String, Int]) {
        (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
      }
    }

    val nGrams = splitToNGrams(n, words)
    countNGrams(nGrams)
  }


  def generateFileNames(directory: String, languages: Map[String, Int], filesExtension: String): Map[String, Seq[String]] = {
    languages.map { case (language, numOfFiles) => {
      val numbers = 0 until numOfFiles - 1
      val filesList = numbers.map(number => directory + language + number + filesExtension)
      (language, filesList)
    }
    }
  }

  def getWordsFromFiles(fileNames: Map[String, Seq[String]], codec: Codec): Map[String, Seq[String]] = {
    def getWordsFromFile(filePath: String, codec: Codec): Iterator[String] = {
      val lines = Source.fromFile(filePath)(codec).getLines()
      val words = lines.flatMap(line => line.split("\\W+").filter(word => !word.isEmpty && word.matches("[^0-9]+")).map(_.toLowerCase))
      words
    }
    fileNames.mapValues(fileNames => {
      fileNames.flatMap(fileName => getWordsFromFile(fileName, codec))
    })
  }

}
