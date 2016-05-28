package pjn.rections

import pjn.wierzba.ScalaDictionaryCLP

object RectionsGenerator {
  def main(args: Array[String]) {
    val dict = new ScalaDictionaryCLP
    val t = dict.indicesOfWord("deszcz")
    println(dict.indicesOfWord("deszcz"))
  }
}
