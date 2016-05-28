package pjn.graphs

object GraphsFiles {
  val pathPrefix = "graphs/out/"

  def graphKFile(k: Int): String = pathPrefix + s"papGraph$k"
}
