package Chat

object Tokens {
  type Token = Int

  // Terms
  val BONJOUR: Token     = 0
  val JE: Token          = 1
  // Actions
  val ETRE: Token        = 2
  val VOULOIR: Token     = 3
  // Operators
  val ET: Token          = 4
  val OU: Token          = 5
  // Products
  val BIERE: Token       = 6
  val CROISSANT: Token   = 7
  // Unknown word
  val UNKNOWN: Token     = 8
  // Utils
  val PSEUDO: Token      = 9
  val NUM: Token         = 10
  val EOL: Token         = 11

  /**
    * Fonction to return the token (String, Token) by getting the string name
    * @param str name of the token
    * @return name of the token and his number
   */
  def getToken(str:String): (String, Token) = str match {
    case "bonjour" => (str , BONJOUR)
    case "je" => (str, JE)
    case "etre" => (str, ETRE)
    case "vouloir" => (str, VOULOIR)
    case "et" => (str, ET)
    case "ou" => (str, OU)
    case "biere" => (str, BIERE)
    case "croissant" => (str, CROISSANT)
    case "eol" => (str, EOL)
    case x if x(0) == '_' => (str, PSEUDO) // is a pseudo (starting by a _)
    case x if x forall Character.isDigit => (str, NUM) // is a number
    case _ => (str, UNKNOWN) // UNKNOWN
  }
}
