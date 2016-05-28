package pjn.graphs

object GraphsFiles {
  val pathPrefix = "graphs/out/"

  def graphKFile(k: Int): String = pathPrefix + s"papGraph$k"

  def graphsSimilarTo(ind: Int): String = pathPrefix + s"graphsSimilarTo$ind"
}
