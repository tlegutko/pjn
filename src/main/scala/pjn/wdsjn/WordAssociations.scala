package pjn.wdsjn

import pjn.io.EasyIO
import pjn.wierzba.ScalaDictionaryCLP

import scala.annotation.tailrec

object Const {
  val inputTextFile = "markov/pap.txt"
  val selectedTextFile = "wdsjn/selected_pap.txt"
  val wordsToAnalyze = Seq("łóżko", "pościel", "sen", "wygodne")
  val textSeparator = "#@#"
  val alpha = 0.66
  val beta = 0.00002
  // todo adjust
  val totalNumOfWords = 11416
  // selectedPap.map(_.size).sum
  val associationsWindowWidth = 12
}

object WordAssociations {

  def main(args: Array[String]): Unit = {
    val dict = new ScalaDictionaryCLP
    val selectedPap = readSelectedSentences(Const.selectedTextFile)
    // TODO calculate occurences
    calculateCooccurrences(selectedPap).foreach(println)
  }

  // TODO add all dictionary forms
  def calculateCooccurrences(selectedPap: Vector[Vector[String]]): Map[(String, String), Int] = {
    def mergeTwoMaps[K, V](z: V, sum: (V, V) => V)(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = {
      m1 ++ m2.map { case (k, v) => k -> sum(v, m1.getOrElse(k, z)) }
    }
    val mergeTwoOccurenceMaps = mergeTwoMaps[(String, String), Int](0, _ + _) _

    def parseSentence(sentence: Vector[String]): Map[(String, String), Int] = {
      def associations(start: Int, end: Int): Map[(String, String), Int] = {
        val range = start to end
        val cartesianProduct = for {
          x <- range
          y <- range if x != y
        } yield (x, y)
        cartesianProduct.foldLeft(Map.empty[(String, String), Int]) {
          case (acc, (left, right)) => acc + ((sentence(left), sentence(right)) -> 1)
        }
      }
      @tailrec
      def moveAssociationWindow(acc: Map[(String, String), Int], left: Int, right: Int): Map[(String, String), Int] = {
        if (left == right) acc
        else {
          val newAcc = mergeTwoOccurenceMaps(acc, associations(left, right))
          moveAssociationWindow(newAcc, left + 1, math.max(right, sentence.size - 1))
        }
      }
      moveAssociationWindow(Map.empty[(String, String), Int], 0, Const.associationsWindowWidth)
    }

    selectedPap.foldLeft(Map.empty[(String, String), Int]) {
      case (acc, sentence) => mergeTwoOccurenceMaps(acc, parseSentence(sentence))
    }
  }

  def readSelectedSentences(inputFileName: String): Vector[Vector[String]] = {
    EasyIO.readLinesFromUTF8FileWithPrefix(inputFileName)
      .mkString(" ")
      .split(Const.textSeparator).toVector
      .map(_.split(" ").filterNot(_ == "").toVector)
  }

  def papToTextsWithWordsToAnalyze(inputFileName: String, selectedTextFile: String, dict: ScalaDictionaryCLP,
                                   wordsToAnalyze: Seq[String]): Unit = {
    val textsToAnalyze = EasyIO.readPAPNotesDepunctuated(inputFileName)
      .filter(sentence => {
        val baseForms = sentence.split(" ").flatMap(dict.indicesOfWord).flatMap(dict.baseForm)
        baseForms.exists(wordsToAnalyze.contains)
      })
    EasyIO.saveToFileWithPrefix[String](selectedTextFile, textsToAnalyze, (x: String) => x + Const.textSeparator)
  }

  def howManyMeanings(fileName: String, dict: ScalaDictionaryCLP): Seq[(Int, Int)] = {
    EasyIO.readPAPNotesDepunctuated(fileName)
      .flatMap(sentence => sentence.split(" "))
      .map(dict.indicesOfWord)
      .map(_.size)
      .groupBy(x => x)
      .map({ case (k, list) => (k, list.size) })
      .toSeq.sortBy(_._1)
  }

}
