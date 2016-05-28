package pjn.graphs

import pjn.io.EasyIO
import pjn.vectors.VectorsFiles

import scala.annotation.tailrec

object GraphGenerator {
  def main(args: Array[String]) {
    val text = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
    val docsOfWords = text.map(_.split(" ").filterNot(_.isEmpty)).filterNot(_.isEmpty)
    val filteredText = filterStopWords(docsOfWords)
    for (k <- 0 to 4) {
      EasyIO.executeAndDisplayElapsedTime({
        val graph = createGraph(filteredText, k)
        saveGraphsToFile(GraphsFiles.graphKFile(k), graph.zipWithIndex)
      }, s"creating $k-graph")
    }
  }

  def filterStopWords(seq: Seq[Array[String]]): Seq[Array[String]] = {
    val wordCount = seq.flatten.foldLeft(Map.empty[String, Int]) { (acc, word) =>
      acc + (word -> (acc.getOrElse(word, 0) + 1))
    }
    val topWords = wordCount.toSeq.sortBy(-_._2).take(100).toMap
    val bottomWords = wordCount.toSeq.sortBy(_._2).takeWhile(_._2 == 1).toMap
    seq.map(_.filterNot(word => topWords.contains(word) || bottomWords.contains(word)))
  }

  def createGraph(docsOfWords: Seq[Array[String]], k: Int): Seq[Map[(String, String), Int]] = {
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

  def compareTwoGraphs(graph1: Map[(String, String), Int], graph2: Map[(String, String), Int]): Double = {
    val product = graph1.foldLeft(0.0) { case (acc, (vertex, value)) =>
      acc + (graph2.getOrElse(vertex, 0).toDouble * value)
    }
    def len(m: Map[(String, String), Int]): Double = {
      val sumOfPows = m.values.map(x => math.pow(x.toDouble, 2)).sum
      math.sqrt(sumOfPows)
    }
    1.0 - product/(len(graph1)*len(graph2))
  }

  def saveGraphsToFile(file: String, seq: Seq[(Map[(String, String), Int], Int)]): Unit = {
    def projection(entry: (Map[(String, String), Int], Int)): String = entry match { case (doc, id) =>
      val docGraph = doc.toSeq.sortBy(-_._2).mkString("\n")
      f"#${id+1}%06d\n$docGraph\n"
    }
    EasyIO.saveToFileWithPrefix(file, seq, projection)
  }

}
