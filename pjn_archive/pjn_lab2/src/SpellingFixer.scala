import java.nio.charset.Charset

import com.google.common.io.Files

object SpellingFixer {

  def main(args: Array[String]) {
    while (true) {
      println("enter word: ")
      val input = scala.io.StdIn.readLine()

      val start = System.currentTimeMillis()
      val suggestion = correctSpellling(input, "formy.txt")
      val timeDiff = (System.currentTimeMillis() - start) / 1000.0
      println(s"$suggestion found in ${timeDiff}s")
    }

  }

  def correctSpellling(input: String, fileName: String): List[(String, Double)] = {
    import java.io._

    import scala.collection.JavaConversions._

    val lines = Files.readLines(new File(fileName), Charset.forName("ISO-8859-2")).toList
    val dictionary = lines.filter(line => line.length >= input.length - 1 && line.length <= input.length + 1)
//    val dictionary = lines.filter(line => heuristicSpeedUp(line, input) && lengthSppedUp(line, input))
    val processedDictionary = dictionary.map(line => (line, calculateModifiedLevenshtein(input, line))).sortBy(_._2)
    val suggestion = processedDictionary.takeWhile(_._2 == processedDictionary.head._2)
    suggestion

  }

  def lengthSppedUp(s: String, in: String): Boolean = {
    s.length >= in.length - 2 && in.length <= in.length + 2
  }

  def heuristicSpeedUp(s: String, in: String): Boolean = {
    s.endsWith(in.last.toString) || s.startsWith(in.head.toString)
  }

  val misspellings = Map(
    "u" -> "�",
    "�" -> "rz",
    "c" -> "ch")

  val diactrics = Map(
    "a" -> "�",
    "e" -> "�",
    "o" -> "�",
    "z" -> "�",
    "z" -> "�",
    "c" -> "�",
    "n" -> "�",
    "l" -> "�")

  val phonetics = Map(
    "f" -> "w",
    "p" -> "b",
    "t" -> "d",
  )


  def calculateModifiedLevenshtein(s1: String, s2: String): Double = {
    import scala.math.{max, min}

    val lev = Array.ofDim[Double](s1.length + 1, s2.length + 1)
    for (i <- 0 to s1.length) {
      for (j <- 0 to s2.length) {
        lev(i)(j) = {
          if (i * j == 0) max(i, j)
          else {
            val minPrev = min(lev(i - 1)(j) + 1, lev(i)(j - 1) + 1)
            val diag = lev(i - 1)(j - 1)
            val twoLetterDiag = diag + twoLettersMetric(s1(i - 1), s2(j - 1))
            val czech = complicatedMetric(i > 1 && j > 1 && s1(i-2) == s2(j-1) && s1(i-1) == s2(j-2), diag)
            val threeLetterA = complicatedMetric(i > 1 && threeLetterMetrics(s1(i-2), s1(i-1), s2(j-1)) == 0.5, lev(i-1)(j))
            val threeLetterB = complicatedMetric(j > 1 && threeLetterMetrics(s2(j-2), s2(j-1), s1(i-1)) == 0.5, lev(i)(j-1))
            Seq(minPrev, twoLetterDiag, czech, threeLetterA, threeLetterB).min
          }
        }
//                print(lev(i)(j) + " ")
      }
//            println()
    }

    lev(s1.length)(s2.length)
  }

  def complicatedMetric(condition: Boolean, currMin: Double): Double = {
    if (condition) currMin - 0.5 else Double.MaxValue
  }

  def twoLettersMetric(aChar: Char, bChar: Char): Double = {
    metric(aChar.toString, bChar.toString)
  }

  def threeLetterMetrics(a1: Char, a2: Char, b1: Char): Double = {
    metric(a1.toString + a2.toString, b1.toString)
  }

  def metric (a: String, b: String): Double = {
    def contains(map: Map[String, String]): Boolean = {
      map.exists(_ ==(a, b)) || map.exists(_ ==(b, a))
    }
    if (a == b)
      0
    else if (contains(diactrics) || contains(misspellings) || contains(phonetics))
      0.5
    else
      1
  }

}
