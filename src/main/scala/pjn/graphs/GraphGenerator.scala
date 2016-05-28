package pjn.graphs

import pjn.io.EasyIO
import pjn.vectors.VectorsFiles

import scala.annotation.tailrec

object GraphGenerator {
  def main(args: Array[String]) {
    val text = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
    val filteredText = preprocessCorpus(text)
    for (k <- 0 to 4) {
      EasyIO.executeAndDisplayElapsedTime({
        val graph = createGraph(filteredText, k)
        saveGraphsToFile(GraphsFiles.graphKFile(k), graph.zipWithIndex)
      }, s"creating $k-graph")
    }
  }

  def preprocessCorpus(text: IndexedSeq[String]): IndexedSeq[Array[String]] = {
    val docsOfWords = text.map(_.split(" ").filterNot(_.isEmpty)).filterNot(_.isEmpty)
    filterStopWords(docsOfWords)
  }

  def filterStopWords(seq: IndexedSeq[Array[String]]): IndexedSeq[Array[String]] = {
    val wordCount = seq.flatten.foldLeft(Map.empty[String, Int]) { (acc, word) =>
      acc + (word -> (acc.getOrElse(word, 0) + 1))
    }
    val topWords = wordCount.toSeq.sortBy(-_._2).take(100).toMap
    val bottomWords = wordCount.toSeq.sortBy(_._2).takeWhile(_._2 == 1).toMap
    seq.map(_.filterNot(word => topWords.contains(word) || bottomWords.contains(word)))
  }

  def createGraph(docsOfWords: IndexedSeq[Array[String]], k: Int): IndexedSeq[Map[(String, String), Int]] = {
    @tailrec
    def graphFromDoc(seq: Seq[String], currMap: Map[(String, String), Int]): Map[(String, String), Int] = {
      if (seq.isEmpty) currMap
      else {
        val currSeq = seq.take(k + 1)
        val newVertices = currSeq.map(word => (currSeq.head, word))
        val newMapEntries = newVertices.map(pair => pair -> (currMap.getOrElse(pair, 0) + 1))
        graphFromDoc(seq.tail, currMap ++ newMapEntries)
      }
    }
    docsOfWords.map(graphFromDoc(_, Map.empty))
  }

  def saveGraphsToFile(file: String, seq: Seq[(Map[(String, String), Int], Int)]): Unit = {
    def projection(entry: (Map[(String, String), Int], Int)): String = entry match { case (doc, id) =>
      val docGraph = doc.toSeq.sortBy(-_._2).mkString("\n")
      f"#${id+1}%06d\n$docGraph\n"
    }
    EasyIO.saveToFileWithPrefix(file, seq, projection)
  }

}
