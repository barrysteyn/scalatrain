/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq

sealed abstract class TrainInfo {
  
  def number: Int
}

sealed case class Schedule(timeTable : Seq[(Time, Station)], day : String = "All", exceptions : Set[java.util.Date] = Set()) {
  //TODO add requires that ensures days are valid (e.g. Mon, Tue etc)	
  def size : Int = timeTable.size
}

case class Train(info: TrainInfo, schedule: Schedule) {
  require(schedule.size >= 2, "schedule must contain at least two elements")

  val stations: Seq[Station] =
    schedule.timeTable map (trainAndStation => trainAndStation._2)

  val departureTime: Map[Station, Time] =
    schedule.timeTable map (timeStation => timeStation.swap)toMap

  val backToBackStations: List[(Station, Station)] =
    stations zip stations.tail toList
  
  override def toString() : String = "$info.number"
}

object TrainInfo {

  case class InterCityExpress(number: Int, hasWifi: Boolean = false) extends TrainInfo

  case class RegionalExpress(number: Int) extends TrainInfo

  case class BavarianRegional(number: Int) extends TrainInfo
}

case class Station(name: String)
