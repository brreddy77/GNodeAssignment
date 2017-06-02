package words

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class WordCounterSpec extends FlatSpec with Matchers with OptionValues {

  behavior of "The wordCount"

  it should "return empty word-count map for an empty list of words" in new Fixture {
    val wordMap = wordCounter.wordCount(Nil)
    wordMap should be (empty)
  }

  it should "return word-count map with words counted correctly" in new Fixture {
    val wordMap = wordCounter.wordCount(words)
    wordMap should not be (empty)
    wordMap should have size (7)
    wordMap.get("aaa").value should equal (2)
    wordMap.get("cc").value should equal (2)
    wordMap.get("zzz") should be (empty)
  }

  trait Fixture {
    val wordCounter = new WordCounter()
    val words: List[String] = "aaa bbbb aaa ccc cc cccc cc 11a 231 ".split("\\W+").toList
  }
}
