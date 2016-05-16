package pjn.zipf

import pjn.io.EasyIO

import scala.annotation.tailrec

object WordCounter {

  val inputFile = EasyIO.resourcesPrefix + "zipf/potop.txt"
  val dictionaryFile = EasyIO.resourcesPrefix + "zipf/odm.txt"
  val wordCountFile = EasyIO.resourcesPrefix + "zipf/wordCount2.txt"
  val diGramStatsFile = EasyIO.resourcesPrefix + "zipf/diGram.txt"
  val triGramStatsFile = EasyIO.resourcesPrefix + "zipf/triGram.txt"

  def main(args: Array[String]): Unit = {
    val lines = EasyIO.readLinesFromISO88592File(dictionaryFile)
    val dictionary = lines.foldLeft(Map.empty[String, String]) {
      (dict, line) => {
        val words = line.split(", ")
        words.tail.foldLeft(dict) { (d, word) => d + (word -> words.head) }
      }
    }
    val words = EasyIO.readLinesFromUTF8File(inputFile).flatMap(_.replaceAll("[\\s.,-;:()!?\"\'`]+", " ").split(" ").filterNot(_ == " "))
    val basicWords = words.map(word => dictionary.getOrElse(word, word))
    val sortedWordCount = basicWords.foldLeft(Map.empty[String, Int]) {
      (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
    }.toList.sortWith(_._2 > _._2).tail
    println("Num of words: " + words.length)
    println("Hapax legomena: " + sortedWordCount.count(_._2 == 1))
    println("Num of words to fill half of text: " + find50p(sortedWordCount, words.length))
    saveStatsToFile(wordCountFile, sortedWordCount)

//    saveStatsToFile(diGramStatsFile, createNGramStatistics(2, words).toList.sortWith(_._2 > _._2))
//    saveStatsToFile(triGramStatsFile, createNGramStatistics(3, words).toList.sortWith(_._2 > _._2))

  }

  def createNGramStatistics(n: Int, words: Seq[String]): Map[String, Int] = {

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

  def find50p(sortedwordCount: List[(String, Int)], size: Int): Int = {
    val halfOfSize = size / 2
    @tailrec
    def find(currSum: Int, values: List[Int], result: Int): Int = {
      if (currSum > halfOfSize) result
      else find(currSum + values.head, values.tail, result + 1)
    }
    find(0, sortedwordCount.map(_._2), 1)
  }

  def saveStatsToFile(fileName: String, stats: Seq[(String, Int)]): Unit = {
    import java.io._
    //    val p = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8), true)
    val p = new PrintWriter(new File(fileName))
    try {
      stats.foreach(s => p.println(s._1 + " " + s._2))
    } finally {
      p.close()
    }
  }

}
