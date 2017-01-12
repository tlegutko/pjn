package pjn.wdsjn

import pjn.io.EasyIO
import pjn.wierzba.ScalaDictionaryCLP

import scala.annotation.tailrec
import scala.collection.mutable

object Const {
  val inputTextFile = "markov/pap.txt"
  val selectedTextFile = "wdsjn/selected_pap.txt"
  val wordsToAnalyze = Seq("łóżko", "pościel", "sen", "wygodny")
  val textSeparator = "#@#"
  val alpha = 0.66
  val beta = 0.0003
  val associationsWindowWidth = 12
  def fileName(primaryWord: String): String =
    s"wdsjn/$primaryWord.csv"
}

object WordAssociations {

  def main(args: Array[String]): Unit = {
    val dict = new ScalaDictionaryCLP
    papToTextsWithWordsToAnalyze(Const.inputTextFile, Const.selectedTextFile, dict, Const.wordsToAnalyze)
    val selectedPap = readSelectedSentences(Const.selectedTextFile)
    val coOccurrences = calculateCoOccurrences(selectedPap, dict)
    val occurrences = calculateOccurrences(selectedPap, dict)
    val totalNumOfWords = selectedPap.map(_.size).sum
    val regulationFactor = totalNumOfWords * Const.beta
    val associationsStrength = coOccurrences.map({
      case ((i, j), hij) =>
        val hj = occurrences(j)
        val rij = {
          if (hj > regulationFactor) hij / math.pow(hj, Const.alpha)
          else hij / regulationFactor
        }
        ((i, j), rij)
    })
    val formattedAssociationsStrength = associationsStrength.groupBy(_._1._1)
      .mapValues(_.map({ case ((_, secondary), value) => (secondary, value) })
        .toList.sortWith(_._2 > _._2))
    formattedAssociationsStrength.foreach { case (primaryWord, associationsList) =>
      EasyIO.saveToFileWithPrefix[(String, Double)](Const.fileName(primaryWord), associationsList, line => f"${line._2}%.2f,${line._1}")
    }
  }

  def mergeTwoMaps[K, V](z: V, sum: (V, V) => V)(m1: mutable.Map[K, V], m2: mutable.Map[K, V]): mutable.Map[K, V] = {
    m1 ++ m2.map { case (k, v) => k -> sum(v, m1.getOrElse(k, z)) }
  }
  private def mergeTwoCoOccurrenceMaps = mergeTwoMaps[(String, String), Int](0, _ + _) _
  private def mergeTwoOccurrenceMaps = mergeTwoMaps[String, Int](0, _ + _) _

  def calculateOccurrences(selectedPap: Vector[Vector[String]], dict: ScalaDictionaryCLP): mutable.Map[String, Int] = {
    selectedPap.foldLeft(mutable.Map.empty[String, Int]) {
      case (acc, sentence) => mergeTwoOccurrenceMaps(acc, sentence.foldLeft(mutable.Map.empty[String, Int]) {
        case (acc2, word) => mergeTwoOccurrenceMaps(acc2, {
          dict.allBaseForms(word).foldLeft(mutable.Map.empty[String, Int]) {
            case (acc3, baseFormWord) => acc3 + (baseFormWord -> 1)
          }
        })
      })
    }
  }


  def calculateCoOccurrences(selectedPap: Vector[Vector[String]], dict: ScalaDictionaryCLP): mutable.Map[(String, String), Int] = {
    def parseSentence(sentence: Vector[String]): mutable.Map[(String, String), Int] = {
      def associations(start: Int, end: Int): mutable.Map[(String, String), Int] = {
        val range = start to end
        val cartesianProduct = for {
          x <- range
          y <- range if x != y
          wordsX <- dict.allBaseForms(sentence(x))
          wordsY <- dict.allBaseForms(sentence(y))
        } yield (wordsX, wordsY)
        cartesianProduct
          .filter(pair => Const.wordsToAnalyze.contains(pair._1))
          .foldLeft(mutable.Map.empty[(String, String), Int]) {
            case (acc, (left, right)) => acc + ((left, right) -> 1)
          }
      }
      @tailrec
      def moveAssociationWindow(acc: mutable.Map[(String, String), Int], left: Int, right: Int): mutable.Map[(String, String), Int] = {
        if (left == right) acc
        else {
          val newAcc = mergeTwoCoOccurrenceMaps(acc, associations(left, right))
          moveAssociationWindow(newAcc, left + 1, math.min(right, sentence.size - 1))
        }
      }
      val startingWindowWidth = math.min(sentence.length-1, Const.associationsWindowWidth)
      moveAssociationWindow(mutable.Map.empty[(String, String), Int], 0, startingWindowWidth)
    }

    selectedPap.foldLeft(mutable.Map.empty[(String, String), Int]) {
      case (acc, sentence) => mergeTwoCoOccurrenceMaps(acc, parseSentence(sentence))
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
