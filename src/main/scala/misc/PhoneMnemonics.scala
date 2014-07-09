/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package misc

class PhoneMnemonics(words: Set[String]) {

  val codeToChars: Map[Char, String] =
    Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
    )

  /**
   * Inverse of codeToChars,
   * e.g. 'A' -> '2', 'B' -> '2'.
   */
  val charCode: Map[Char, Char] =
    for {
      (digit, chars) <- codeToChars
      char <- chars
    } yield char -> digit

  /**
   * Map a word to its digit representation,
   * e.g. "Java" -> "5282".
   */
  def wordCode(word: String): String =
    word.toUpperCase map charCode

  /**
   * Group all words by their number representations,
   * e.g. "5282" -> Set("Java", "Kata", ...).
   */
  val wordsByNumber: Map[String, Set[String]] =
    words groupBy wordCode

  /**
   * All ways to encode a number as a phrase, i.e. as a sequence of words.
   */
  def encode(number: String): Set[Seq[String]] =
    if (number.isEmpty)
      Set(Seq.empty)
    else {
      val splitPoints = (1 to number.length).toSet
      for {
        splitPoint <- splitPoints
        prefix = number take splitPoint
        suffix = number drop splitPoint
        prefixWord <- wordsByNumber.getOrElse(prefix, Set())
        suffixPhrase <- encode(suffix)
      } yield prefixWord +: suffixPhrase
    }

  /**
   * Maps a number to the set of all phrases that can represent it.
   */
  def translate(number: String): Set[String] =
    encode(number) map (phrase => phrase mkString " ")
}
