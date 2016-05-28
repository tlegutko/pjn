package pjn.graphs

import pjn.io.EasyIO
import pjn.vectors.VectorsFiles

import scala.util.Random

object GraphComparator {
  def main(args: Array[String]) {
    val text = EasyIO.readPAPNotes(VectorsFiles.pap)
    val textDepunctuated = EasyIO.readPAPNotesDepunctuated(VectorsFiles.pap)
    val filteredText = GraphGenerator.preprocessCorpus(textDepunctuated)

    1 to 10 foreach { i => EasyIO.executeAndDisplayElapsedTime({
      val selectedDocId = Random.nextInt(text.length)
      val similarDocsToSave = 0 to 4 map { k =>
        val kGraph = GraphGenerator.createGraph(filteredText, k)
        val graphPartiallyCompared = compareTwoGraphs(kGraph(selectedDocId), _: Map[(String, String), Int])
        val similarDocs = kGraph.map(graphPartiallyCompared).zipWithIndex
          .filterNot(_._2 == selectedDocId).sortBy(_._1).take(10)
        val similarDocsToDisplay = similarDocs.map { case (metric, index) =>
          (metric, index + 1, text(index + 1))
        }
        (k, similarDocsToDisplay)
      }
      saveToFile(GraphsFiles.graphsSimilarTo(selectedDocId + 1), similarDocsToSave, (selectedDocId + 1, text(selectedDocId + 1)))
    }, s"generating similarNotes #$i")
    }
  }

  def compareTwoGraphs(graph1: Map[(String, String), Int], graph2: Map[(String, String), Int]): Double = {
    val product = graph1.foldLeft(0.0) { case (acc, (vertex, value)) =>
      acc + (graph2.getOrElse(vertex, 0).toDouble * value)
    }
    def len(m: Map[(String, String), Int]): Double = {
      val sumOfPows = m.values.map(x => math.pow(x.toDouble, 2)).sum
      math.sqrt(sumOfPows)
    }
    1.0 - product / (len(graph1) * len(graph2))
  }

  def saveToFile(file: String, seq: IndexedSeq[(Int, IndexedSeq[(Double, Int, String)])], header: (Int, String)): Unit = {
    def projection(elem: (Int, IndexedSeq[(Double, Int, String)])): String = {
      elem match {
        case (k, docs) => s"k=$k\n${docs.mkString("\n")}"
      }
    }
    val headerString = header match {
      case (docId, text) =>
        s"${EasyIO.papNumber(docId + 1)}\n$text"
    }
    EasyIO.saveToFileWithPrefix(file, seq, projection, headerString)
  }
}
