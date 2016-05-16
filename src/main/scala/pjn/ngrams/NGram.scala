package pjn.ngrams

sealed trait NGram {
  val str: String

  override def toString = str

  def last: String
}

sealed trait NGramBuilder {
  def splitToNGrams(nGramLength: Int, texts: Seq[String]): Seq[NGram]
}

case class SentenceNGram(str: String) extends NGram {
  override def last: String = str.split(" ").last
}

object SentenceNGram extends NGramBuilder {
  override def splitToNGrams(nGramLength: Int, texts: Seq[String]): Seq[NGram] = {
    for {
      text <- texts
      words = text.split(" ")
      lowerBound <- 0 to words.length - nGramLength
      upperBound = lowerBound + nGramLength
      resultingNGram = words.slice(lowerBound, upperBound).mkString(" ")
    } yield new SentenceNGram(resultingNGram)
  }

  def splitToNGrams2(nGramLength: Int, texts: Seq[String]): Seq[Seq[NGram]] = {
    def splitLine(text: String): Seq[NGram] = {
      val words = text.split(" ")
      for {
        lowerBound <- 0 to words.length - nGramLength
        upperBound = lowerBound + nGramLength
        resultingNGram = words.slice(lowerBound, upperBound).mkString(" ")
      } yield new SentenceNGram(resultingNGram)
    }
    texts.map(splitLine)
  }
}

case class WordNGram(str: String) extends NGram {
  override def last: String = str.last.toString
}

object WordNGram extends NGramBuilder {
  override def splitToNGrams(nGramLength: Int, texts: Seq[String]): Seq[NGram] = {
    for {
      text <- texts
      word <- text.split(" ")
      lowerBound <- 0 to word.length - nGramLength
      upperBound = lowerBound + nGramLength
      resultingNGram = word.substring(lowerBound, upperBound)
    } yield new WordNGram(resultingNGram)
  }
}

