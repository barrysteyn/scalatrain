/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import scala.collection.immutable.Seq
import com.typesafe.training.scalatrain.WeekDays.WeekDay

import java.util.{ Calendar, Date }

sealed abstract class TrainInfo {

  def number: Int
}

object WeekDays extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun, All = Value
  def intToWeekDay(dow: Int): WeekDay = {
    require(dow >= 1 && dow <= 7, "Invalid day of week")
    dow match {
      case 1 => Sun
      case 2 => Mon
      case 3 => Tue
      case 4 => Wed
      case 5 => Thu
      case 6 => Fri
      case 7 => Sat
    }
  }
}

sealed case class Schedule(timeTable: Seq[(Time, Station)], day: WeekDay = WeekDays.All, exceptions: Set[Date] = Set()) {
  def size: Int = timeTable.size
  def validForDate(date: Date): Boolean = {
    val calendar = Calendar.getInstance()
    calendar.setTime(date)
    val dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK)
    if (day != WeekDays.All && day != WeekDays.intToWeekDay(dayOfWeek)) false
    else if (exceptions contains date) false
    else true
  }
}

case class Train(info: TrainInfo, schedule: Schedule) {
  require(schedule.size >= 2, "schedule must contain at least two elements")

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
