package pjn.wierzba

import scala.collection.JavaConverters._
import scala.language.implicitConversions

class ScalaDictionaryCLP {
  val javaDictionaryCLP = new DictionaryCLP()

  def indexesOfWord(word: String): List[Int] = javaDictionaryCLP.clp_rec(word)

  private implicit def javaListOfStringToScala(l: java.util.List[String]): List[String] = l.asScala.toList

  private implicit def javaListOfIntsToScala(l: java.util.List[Integer]): List[Int] = l.asScala.map(_.intValue()).toList
}
