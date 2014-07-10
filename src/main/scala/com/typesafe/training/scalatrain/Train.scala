/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq

sealed abstract class TrainInfo {

  def number: Int
}

case class Train(info: TrainInfo, schedule: Seq[(Time, Station)]) {
  require(schedule.size >= 2, "schedule must contain at least two elements")

  val stations: Seq[Station] =
    schedule map (trainAndStation => trainAndStation._2)

  val departureTime: Map[Station, Time] =
    schedule map (timeStation => timeStation.swap)toMap

  val backToBackStations: List[(Station, Station)] =
    stations zip stations.tail toList
}

object TrainInfo {

  case class InterCityExpress(number: Int, hasWifi: Boolean = false) extends TrainInfo

  case class RegionalExpress(number: Int) extends TrainInfo

  case class BavarianRegional(number: Int) extends TrainInfo
}

case class Station(name: String)
