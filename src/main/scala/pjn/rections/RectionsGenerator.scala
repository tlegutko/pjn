package pjn.rections

import pjn.io.EasyIO
import pjn.vectors.VectorsFiles
import pjn.wierzba.DictionaryCLP.WordType
import pjn.wierzba.ScalaDictionaryCLP

import scala.annotation.tailrec

object RectionsGenerator {
  def main(args: Array[String]) {
    val dict = new ScalaDictionaryCLP
    val text = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
    val docsOfWords = text.map(_.split(" ").filterNot(_.isEmpty)).filterNot(_.isEmpty)
    EasyIO.executeAndDisplayElapsedTime(
      saveToFile("rections/out/rections", findRections(docsOfWords, dict)),
      "calculating rections"
    )
  }

  def findRections(docsOfWords: IndexedSeq[Array[String]], dict: ScalaDictionaryCLP): Map[String, Map[WordType, Double]] = {
    def isPreposition(w: String): Boolean =
      dict.indicesOfWord(w).map(dict.wordType).exists(_ == WordType.PRZYIMEK)
    def wordTypes(w: String): List[WordType] = {
      dict.indicesOfWord(w).map(dict.wordType)
    }

    @tailrec
    def countRectionsInDoc(seq: List[String], rections: Map[String, Map[WordType, Int]]): Map[String, Map[WordType, Int]] = {
      seq match {
        case head :: second :: tail if isPreposition(head) => {
          val prepMap = rections.getOrElse(head, Map.empty)
          val newVertices = wordTypes(second).map(w => w -> (prepMap.getOrElse(w, 0) + 1))
          val newPrepMap = prepMap ++ newVertices
          countRectionsInDoc(seq.tail, rections + (head -> newPrepMap))
        }
        case Nil => rections
        case _ => countRectionsInDoc(seq.tail, rections)
      }
    }

    val mapsPerDoc = docsOfWords.map(doc => countRectionsInDoc(doc.toList, Map.empty))
    val unifiedMap = mapsPerDoc.foldLeft(Map.empty[String, Map[WordType, Int]]) { (acc, newMap) =>
      acc ++ newMap.map { case (k, v) =>
        k -> {
          val nestedMap = acc.getOrElse(k, Map.empty)
          nestedMap ++ v.map { case (k2, v2) => k2 -> (nestedMap.getOrElse(k2, 0) + v2) }
        }
      }
    }
    unifiedMap.map { case (wordType, rections) =>
      val sum = rections.values.sum.toDouble
      (wordType, rections.mapValues(_ / sum))
    }
  }

  def saveToFile(file: String, seq: Map[String, Map[WordType, Double]]): Unit = {
    def projection(m: (String, Map[WordType, Double])): String = m match {
      case (word, values) => {
        val sortedValues = values.toSeq.sortBy(-_._2)
        s"$word -> ${sortedValues.mkString(", ")}"
      }
    }
    EasyIO.saveToFileWithPrefix(file, seq.toSeq, projection)
  }

}
