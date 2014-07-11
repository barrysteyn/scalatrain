/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain
import java.util.{ Calendar, Date }

object TestData {

  val munich = Station("Munich")

  val nuremberg = Station("Nuremberg")

  val frankfurt = Station("Frankfurt")

  val cologne = Station("Cologne")

  val essen = Station("Essen")

  val ice724MunichTime = Time(8, 50)

  val ice724NurembergTime = Time(10)

  val ice724FrankfurtTime = Time(12, 10)

  val ice724CologneTime = Time(13, 39)

  val ice726MunichTime = Time(7, 50)

  val ice726NurembergTime = Time(9)

  val ice726FrankfurtTime = Time(11, 10)

  val ice726CologneTime = Time(13, 2)

  val ice724schedule = Schedule(
    Vector(
      ice724MunichTime -> munich,
      ice724NurembergTime -> nuremberg,
      ice724FrankfurtTime -> frankfurt,
      ice724CologneTime -> cologne
    ),
    WeekDays.Mon,
    Set(new Date(2014, 11, 11))
  )

  val ice726schedule = Schedule(
    Vector(
      ice726MunichTime -> munich,
      ice726NurembergTime -> nuremberg,
      ice726FrankfurtTime -> frankfurt,
      ice726CologneTime -> essen
    ),
    WeekDays.All,
    Set(new Date(2014, 12, 12))
  )

  val ice724 = Train(TrainInfo.InterCityExpress(724), ice724schedule, 100)

  val ice726 = Train(TrainInfo.InterCityExpress(726), ice726schedule, 200)

  val hopMunichNuremberg724 = Hop(munich, nuremberg, ice724)
  val hopNurembergFrankfurt724 = Hop(nuremberg, frankfurt, ice724)
  val hopFrankfurtCologne724 = Hop(frankfurt, cologne, ice724)
  val hopMunichNuremberg726 = Hop(munich, nuremberg, ice726)
  val hopNurembergFrankfurt726 = Hop(nuremberg, frankfurt, ice726)
  val hopFrankfurtEssen726 = Hop(frankfurt, essen, ice726)

  val ice724path = Path(List(hopMunichNuremberg724, hopNurembergFrankfurt724, hopFrankfurtCologne724))
  val ice726path = Path(List(hopMunichNuremberg726, hopNurembergFrankfurt726, hopFrankfurtEssen726))

  val planner = new JourneyPlanner(Set(ice724, ice726))
}
