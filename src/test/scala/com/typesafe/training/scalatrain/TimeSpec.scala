/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import java.lang.{ IllegalArgumentException => IAE }
import org.scalatest.{ Matchers, WordSpec }
import scala.util.parsing.json.{ JSONObject => JsonObject }

class TimeSpec extends WordSpec with Matchers {

  "Calling fromJson" should {
    "return None for an invalid JsonOject" in {
      Time fromJson JsonObject(Map()) shouldBe None
    }
    "return Some wrapping a properly initialized Time for a valid JsonOject" in {
      (Time fromJson JsonObject(Map("hours" -> 9, "minutes" -> 30))) shouldEqual Some(Time(9, 30))
    }
  }

  "Calling fromJson after toJson" should {
    "return Some wrapping the original Time" in {
      val time = Time(9, 30)
      (Time fromJson (time.toJson)) shouldEqual Some(time)
    }
  }

  "Creating a Time" should {
    "throw an IllegalArgumentException for hours not within 0 and 23" in {
      an[IAE] should be thrownBy Time(-1)
      an[IAE] should be thrownBy Time(24)
    }
    "throw an IllegalArgumentException for minutes not within 0 and 59" in {
      an[IAE] should be thrownBy Time(minutes = -1)
      an[IAE] should be thrownBy Time(minutes = 60)
    }
  }

  "The default arguments for hours and minutes" should {
    "be equal to 0" in {
      val time = Time()
      time.hours shouldEqual 0
      time.minutes shouldEqual 0
    }
  }

  "asMinutes" should {
    "be initialized correctly" in {
      Time(1, 40).asMinutes shouldEqual 100
    }
  }

  "Calling minus or -" should {
    "return the correct difference in minutes" in {
      Time(1, 40) minus Time(1, 10) shouldEqual 30
      Time(1, 40) - Time(1, 10) shouldEqual 30
    }
  }

  "Calling toString" should {
    "return a properly formatted string representation" in {
      Time(9, 30).toString shouldEqual "09:30"
    }
  }

  "Calling Ordered operators" should {
    "work as expected" in {
      Time() < Time(1) shouldBe true
      Time() >= Time(1) shouldBe false
    }
  }
}
