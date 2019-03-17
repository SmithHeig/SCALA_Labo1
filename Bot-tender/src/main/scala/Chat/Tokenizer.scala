/**
  * Scala: Labo01 - Step 3 - Tokenizer
  *
  * Auteurs: J.Châtillon, J.Smith
  * Date: 17.03.2019
  */

package Chat

import Tokens._
import Utils.SpellChecker._
class Tokenizer(input: String) {

  private var tokens = Array[(String,Token)]()
  private var index = -1 // initialise index to -1 because will be increase before accessing first time
  /**
    * Separate the user's input into tokens.
    */
  def tokenize(): Unit = {
    tokens = input
      .toLowerCase() // Mettre tous en minuscule. Did that first to lowercase letters with accent too ex: Ê
      .replaceAll("[!?*,.']","") // replace all ponctuation by nothing
      .replaceAll("[éèê]","e") // remove all accent on e
      .replaceAll("[àä]", "a") // remove all accent on a
      .split(" ") // split every word of the sentence
      .map(x => getToken(getClosestWordInDictionary(x))) // get the closest word in our dico
  }

  /**
    * Get the next token of the user input, or OEL if there is no more token.
  	* @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token) = {
    index += 1 // Go to next token
    if(index < tokens.length) tokens(index) else getToken("eol") // return the token or EOL if finish
  }
}
