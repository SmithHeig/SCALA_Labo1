package Utils
import scala.math.min

object SpellChecker {
  /**
    * Calculate the Levenshtein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
    */
  // TODO - Step 2
  def stringDistance(s1: String, s2: String): Int = {
    def loop(s1 : List[Char], s2 : List[Char], acc: Int) : Int = {
      (s1, s2) match {
        case (Nil, Nil) => acc
        case (_ , Nil) => acc + s1.length
        case (Nil, _) => acc + s2.length
        case (c1::t1, c2::t2) if c1 == c2 => min(min(loop(t1,s2, acc), loop(s1, t2,acc)), loop(t1,t2,acc))
        case (c1::t1, c2::t2) => min(min(loop(t1,s2, acc + 1), loop(s1, t2,acc + 1)), loop(t1,t2,acc + 1))
      }
    }
    loop(s1.toList,s2.toList,0)
  }

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String =  misspelledWord match {
    case str if str forall Character.isDigit => str
    case str if str(0) == '_' => str
    case str => Dictionary.dictionary.mapValues(x => stringDistance(str, x)).keysIterator.max
  }
}
