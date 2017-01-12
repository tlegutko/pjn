package pjn.wierzba

import pjn.wierzba.DictionaryCLP.WordType

import scala.collection.JavaConverters._
import scala.language.implicitConversions

class ScalaDictionaryCLP {
  val javaDictionaryCLP = new DictionaryCLP()

  def indicesOfWord(word: String): List[Int] = javaDictionaryCLP.clp_rec(word)

  def wordType(id: Int): WordType = javaDictionaryCLP.clp_pos(id)

  def baseForm(id: Int): List[String] = javaDictionaryCLP.clp_bform(id)

  def allBaseForms(word: String): List[String] = indicesOfWord(word).flatMap(baseForm)

  private implicit def javaListOfStringToScala(l: java.util.List[String]): List[String] = l.asScala.toList

  private implicit def javaListOfIntsToScala(l: java.util.List[Integer]): List[Int] = l.asScala.map(_.intValue()).toList
}
