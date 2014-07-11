/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import TestData._
import java.lang.{ IllegalArgumentException => IAE }
import org.scalatest.{ Matchers, WordSpec }
import java.util.Date

class TrainSpec extends WordSpec with Matchers {

  "Creating a Train" should {
    "throw an IllegalArgumentException for a schedule with 0 or 1 elements" in {
      an[IAE] should be thrownBy Train(TrainInfo.InterCityExpress(724), Schedule(Vector()))
      an[IAE] should be thrownBy Train(TrainInfo.InterCityExpress(724), Schedule(Vector(ice724MunichTime -> munich)))
    }
  }

  "stations" should {
    "be initialized correctly" in {
      ice724.stations shouldEqual Vector(munich, nuremberg, frankfurt, cologne)
    }
  }

  "backToBackStations" should {
    "be initialized correctly" in {
      ice724.backToBackStations shouldEqual List((munich, nuremberg), (nuremberg, frankfurt), (frankfurt, cologne))
    }
  }

  "validForDate" should {
    "Return true for a schedule that is valid for all days and is not on the exception list" in {
      ice726schedule.validForDate(new Date(2014, 1, 1)) shouldEqual true

    }
    "Return false if the day passed in does not match the day of the schedule" in {
      ice724schedule.validForDate(new Date(2014, 7, 14)) shouldEqual false

    }
    "Return false if the date passed in is on the schedule's exception list" in {
      ice726schedule.validForDate(new Date(2014, 12, 12)) shouldEqual false
    }
  }
}
