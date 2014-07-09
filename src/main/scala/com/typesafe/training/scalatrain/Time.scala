/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

import scala.util.control.Exception
import scala.util.parsing.json.{ JSONObject => JsonObject }

object Time {

  def fromMinutes(minutes: Int): Time =
    Time(minutes / 60, minutes % 60)

  def fromJson(json: JsonObject): Option[Time] =
    for {
      hoursAny <- json.obj get "hours"
      hours <- Exception.allCatch opt hoursAny.toString.toInt
      minutesAny <- json.obj get "minutes"
      minutes <- Exception.allCatch opt minutesAny.toString.toInt
    } yield Time(hours, minutes)
}

case class Time(hours: Int = 0, minutes: Int = 0) extends Ordered[Time] {
  require(hours >= 0 && hours < 24, "hours must be within 0 and 23")
  require(minutes >= 0 && minutes < 60, "minutes must be within 0 and 59")

  val asMinutes: Int =
    hours * 60 + minutes

  override lazy val toString: String =
    f"$hours%02d:$minutes%02d"

  def minus(that: Time): Int =
    this.asMinutes - that.asMinutes

  def -(that: Time): Int =
    minus(that)

  // TODO This "pollutes" the API; in the Advanced Scala course we learn a better solution
  def toJson: JsonObject =
    JsonObject(Map("hours" -> hours, "minutes" -> minutes))

  override def compare(that: Time): Int =
    this - that
}
