package pjn.rections

import pjn.io.EasyIO
import pjn.vectors.VectorsFiles
import pjn.wierzba.DictionaryCLP.WordType
import pjn.wierzba.ScalaDictionaryCLP

import scala.annotation.tailrec
import scalaz.Scalaz._

object RectionsGenerator {
  def main(args: Array[String]) {
    val dict = new ScalaDictionaryCLP
    val text = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
    val docsOfWords = text.map(_.split(" ").filterNot(_.isEmpty)).filterNot(_.isEmpty)
    EasyIO.executeAndDisplayElapsedTime(
      saveToFile("rections/out/rections3", findRections(docsOfWords, dict)),
      "calculating rections3"
    )
  }

  def findRections(docsOfWords: IndexedSeq[Array[String]], dict: ScalaDictionaryCLP): IndexedSeq[(String, Map[WordType, Double])] = {
    def isPreposition(w: String): Boolean =
      dict.indicesOfWord(w).map(dict.wordType).contains(WordType.PRZYIMEK)
    def wordTypes(w: String): List[WordType] = {
      dict.indicesOfWord(w).map(dict.wordType)
    }

    @tailrec
    def countRectionsInDoc(seq: List[String], rections: Map[String, Map[WordType, Int]]): Map[String, Map[WordType, Int]] = {
      seq match {
        case head :: second :: tail if isPreposition(head) =>
          val prepMap = rections.getOrElse(head, Map.empty)
          val newVertices = wordTypes(second).map(w => w -> (prepMap.getOrElse(w, 0) + 1))
          val newPrepMap = prepMap ++ newVertices
          countRectionsInDoc(seq.tail, rections + (head -> newPrepMap))
        case Nil => rections
        case _ => countRectionsInDoc(seq.tail, rections)
      }
    }

    val mapsPerDoc = docsOfWords.map(doc => countRectionsInDoc(doc.toList, Map.empty))
    val unifiedMap = mapsPerDoc reduce (_ |+| _)
    val sortedMapWithMetrics = unifiedMap.map { case (wordType, rections) =>
      val sum = rections.values.sum
      (wordType, rections.mapValues(_ / sum.toDouble), sum)
    }.toVector.filter(_._3 > 6).sortBy(-_._3)
    sortedMapWithMetrics.map { case (wordType, rections, sum) => (wordType, rections) }
  }

  def unifyMapsManually(seq: Seq[Map[String, Map[WordType, Int]]]): Map[String, Map[WordType, Int]] = {
    seq reduce { (acc, newMap) =>
      acc ++ newMap.map { case (k, v) =>
        val nestedMap = acc.getOrElse(k, Map.empty)
        k -> (nestedMap ++ v.map { case (k2, v2) => k2 -> (nestedMap.getOrElse(k2, 0) + v2) })
      }
    }
  }

  def unifyTwoMapsRecursively(m1: Map[String, Map[WordType, Int]], m2: Map[String, Map[WordType, Int]]): Map[String, Map[WordType, Int]] = {
    def unifyTwoMaps[K, V](nestedMapOps: (V, (V, V) => V))(m1: Map[K, V], m2: Map[K, V]): Map[K, V] = {
      nestedMapOps match {
        case (zero, add) =>
          m1 ++ m2.map { case (k, v) => k -> add(m1.getOrElse(k, zero), v) }
      }
    }
    val intOps = (0, (a: Int, b: Int) => a + b)
    val mapOps = (Map.empty[WordType, Int], unifyTwoMaps(intOps)(_: Map[WordType, Int], _: Map[WordType, Int]))
    unifyTwoMaps(mapOps)(m1, m2)
  }


  def saveToFile(file: String, seq: IndexedSeq[(String, Map[WordType, Double])]): Unit = {
    def projection(m: (String, Map[WordType, Double])): String = m match {
      case (word, values) =>
        val sortedValues = values.toSeq.sortBy(-_._2)
        s"$word -> ${sortedValues.mkString(", ")}"
    }
    EasyIO.saveToFileWithPrefix(file, seq, projection)
  }

}
