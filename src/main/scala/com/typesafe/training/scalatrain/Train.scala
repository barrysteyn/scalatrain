/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq

import java.util.{ Calendar, Date }

sealed abstract class TrainInfo {

  def number: Int
}

object WeekDays extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun, All = Value
  val calendar = Calendar.getInstance()

  def intToWeekDay(date: Date): WeekDay = {
    calendar.setTime(date)
    val dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK)
    dayOfWeek match {
      case 5 => Sun
      case 6 => Mon
      case 7 => Tue
      case 1 => Wed
      case 2 => Thu
      case 3 => Fri
      case 4 => Sat
    }
  }
}

sealed case class Schedule(timeTable: Seq[(Time, Station)], day: WeekDays.Value = WeekDays.All, exceptions: Set[Date] = Set()) {
  def size: Int = timeTable.size
  def validForDate(date: Date): Boolean = {
    if (day != WeekDays.All && day != WeekDays.intToWeekDay(date)) false
    else if (exceptions contains date) false
    else true
  }
}

case class Train(info: TrainInfo, schedule: Schedule, cost : Double) {
  require(schedule.size >= 2, "schedule must contain at least two elements")
 
  val costPerHop : Double = cost/schedule.size
  
  val stations: Seq[Station] =
    schedule.timeTable map (trainAndStation => trainAndStation._2)

  val departureTime: Map[Station, Time] =
    schedule.timeTable map (timeStation => timeStation.swap)toMap

  val backToBackStations: List[(Station, Station)] =
    stations zip stations.tail toList

  override def toString(): String = s"$info.number"
}

object TrainInfo {

  case class InterCityExpress(number: Int, hasWifi: Boolean = false) extends TrainInfo

  case class RegionalExpress(number: Int) extends TrainInfo

  case class BavarianRegional(number: Int) extends TrainInfo
}

case class Station(name: String)
