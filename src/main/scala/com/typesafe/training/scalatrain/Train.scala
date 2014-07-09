/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq

case class Train(info: TrainInfo, schedule: Seq[(Time, Station)]) {
  require(schedule.size >= 2, "schedule must contain at least two elements")
  // TODO Verify that `schedule` is strictly increasing in time

  val stations: Seq[Station] =
    // Could also be expressed in short notation: schedule map (_._2)
    schedule map (trainAndStation => trainAndStation._2)

  def backToBackStations: List[(Station, Station)] =
    for {
      fromTo <- stations sliding (2, 1) toList
    } yield (fromTo(0), fromTo(1))

  def departureTime(station: Station): (Station, Time) = {
    val stationTime = schedule filter (s => s._2 == station)
    (stationTime(0)._2, stationTime(0)._1)
  }
}

object TrainInfo {

  case class InterCityExpress(number: Int, hasWifi: Boolean = false) extends TrainInfo

  case class RegionalExpress(number: Int) extends TrainInfo

  case class BavarianRegional(number: Int) extends TrainInfo
}

sealed abstract class TrainInfo {

  def number: Int
}

case class Station(name: String)

case class Hop(from: Station, to: Station, train: Train) {
  val (_, departureTime) = train departureTime from
  val (_, arrivalTime) = train departureTime to

}
