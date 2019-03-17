/**
  * Scala: Labo01 - Step 1 - Clinks Calculator
  *
  * Auteurs: J.Châtillon, J.Smith
  * Date: 17.03.2019
  */

package Utils

/**
  * Contains the functions necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator {
  /**
    * Calculate the factorial of a given number.
    * @param n the number to compute
    * @return n!
    */
  def factorial(n: Int): Int = {
    def loop(n: Int, acc: Int) : Int = {
      if(n == 0) acc else loop(n -1, acc * n)
    }
    if(n < 0) throw new Error("Factoriel works only with positive numbers") else loop(n,1)
  }

  /**
    * Calculate the combination of two given numbers.
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  def calculateCombination(n: Int, k: Int): Int = {
    factorial(n) / (factorial(k) * factorial(n-k))
  }
}
