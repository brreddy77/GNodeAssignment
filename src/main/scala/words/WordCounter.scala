package words

import scala.io.Source

class WordCounter {

  def wordCount(words: List[String]): Map[String, Int] = {
    words.foldLeft(Map.empty[String, Int]) { (wordMap, word) =>
      val currentCount = wordMap.getOrElse(word, 0)
      wordMap.updated(word, currentCount + 1)
    }
  }

  def processFile(file: String): Map[String, Int] = {
    val words = for {
      line <- Source.fromFile(file).getLines
      word <- line.split("\\W+")
    } yield word

    wordCount(words.toList)
  }
}

object WordCounterApp extends App {
  val wordCounter = new WordCounter()

  val wordMap = wordCounter.processFile("TestFile.txt")
  println(wordMap)
}