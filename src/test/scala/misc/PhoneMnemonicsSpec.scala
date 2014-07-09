/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package misc

import org.scalatest.{ Matchers, WordSpec }

class PhoneMnemonicsSpec extends WordSpec with Matchers {

  "charCode" should {
    "be correctly initialized" in {
      phoneMnemonics.charCode shouldEqual Map(
        'A' -> '2', 'B' -> '2', 'C' -> '2', 'D' -> '3', 'E' -> '3', 'F' -> '3',
        'G' -> '4', 'H' -> '4', 'I' -> '4', 'J' -> '5', 'K' -> '5', 'L' -> '5',
        'M' -> '6', 'N' -> '6', 'O' -> '6', 'P' -> '7', 'Q' -> '7', 'R' -> '7', 'S' -> '7',
        'T' -> '8', 'U' -> '8', 'V' -> '8', 'W' -> '9', 'X' -> '9', 'Y' -> '9', 'Z' -> '9'
      )
    }
  }

  "Calling wordCode" should {
    "return the correct word code" in {
      (phoneMnemonics wordCode java) shouldEqual "5282"
    }
  }

  "wordsByNumber" should {
    "be correctly initialized" in {
      phoneMnemonics.wordsByNumber shouldEqual Map("5282" -> Set(java, kata))
    }
  }

  "Calling encode" should {
    "return the correct set of words" in {
      (phoneMnemonics encode "5282") shouldEqual Set(Seq(java), Seq(kata))
      (phoneMnemonics encode "52825282") shouldEqual Set(
        Seq(java, kata), Seq(java, java), Seq(kata, java), Seq(kata, kata)
      )
      (phoneMnemonics encode "123") shouldEqual Set.empty
    }
  }

  private val java = "Java"

  private val kata = "Kata"

  private val phoneMnemonics = new PhoneMnemonics(Set(java, kata))
}
