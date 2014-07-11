/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import TestData._
import java.lang.{ IllegalArgumentException => IAE }
import org.scalatest.{ Matchers, WordSpec }

import scala.collection.immutable.Seq

class JourneyPlannerSpec extends WordSpec with Matchers {

  "stations" should {
    "be initialized correctly" in {
      planner.stations shouldEqual Set(munich, nuremberg, frankfurt, cologne, essen)
    }
  }

  "Calling trainsAt" should {
    "return the correct trains" in {
      planner.trainsAt(munich) shouldEqual Set(ice724, ice726)
      planner.trainsAt(cologne) shouldEqual Set(ice724)
    }
  }

  "Calling stopsAt" should {
    "return the correct stops" in {
      planner.stopsAt(munich) shouldEqual Set(ice724MunichTime -> ice724, ice726MunichTime -> ice726)
    }
  }

  "Calling isShortTrip" should {
    "return false for more than one station in between" in {
      planner.isShortTrip(munich, cologne) shouldBe false
      planner.isShortTrip(munich, essen) shouldBe false
    }
    "return true for zero or one stations in between" in {
      planner.isShortTrip(munich, nuremberg) shouldBe true
      planner.isShortTrip(munich, frankfurt) shouldBe true
      planner.isShortTrip(nuremberg, frankfurt) shouldBe true
      planner.isShortTrip(nuremberg, essen) shouldBe true
    }
  }

  "Calculating paths" should {
    "between two stations" should {
      planner.getPathsAtTime(munich, nuremberg, ice724MunichTime) shouldEqual Set[List[Hop]](List(hopMunichNuremberg724))
      planner.getPathsAtTime(munich, nuremberg, ice726MunichTime) shouldEqual Set[List[Hop]](List(hopMunichNuremberg724), List(hopMunichNuremberg726))
      planner getPathsAtTime (munich, frankfurt, ice726MunichTime) shouldEqual Set[List[Hop]](
        List(hopMunichNuremberg724, hopNurembergFrankfurt724),
        List(hopMunichNuremberg726, hopNurembergFrankfurt726),
        List(hopMunichNuremberg726, hopNurembergFrankfurt724)
      )
    }
  }

  "departingHopsAtTime" should {
    "return a set of hops for a given station, as long as there are trains leaving at or later than the departure time" in {
      planner.departingHopsAtTime(nuremberg, Time(9)) should contain(hopNurembergFrankfurt726)
      planner.departingHopsAtTime(nuremberg, Time(9)) should contain(hopNurembergFrankfurt724)
    }
  }

  "sortPaths" should {
    "sort paths by total time in ascending order" in {
      val sortedPaths = planner.sortPathsByTime(planner getPathsAtTime (munich, frankfurt, ice726MunichTime))
      val possibleSortedPaths = List(
        Vector(
          List(hopMunichNuremberg724, hopNurembergFrankfurt724),
          List(hopMunichNuremberg726, hopNurembergFrankfurt726),
          List(hopMunichNuremberg726, hopNurembergFrankfurt724)

        ),
        Vector(
          List(hopMunichNuremberg726, hopNurembergFrankfurt726),
          List(hopMunichNuremberg724, hopNurembergFrankfurt724),
          List(hopMunichNuremberg726, hopNurembergFrankfurt724)
        )
      )
      possibleSortedPaths should contain(sortedPaths)
    }

    "sort paths by total cost in ascending order" in {

    }
  }
}
