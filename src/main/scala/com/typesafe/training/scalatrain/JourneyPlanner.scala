/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

class JourneyPlanner(trains: Set[Train]) {

  val stations: Set[Station] =
    // Could also be expressed in short notation: trains flatMap (_.stations)
    trains flatMap (train => train.stations)

  def trainsAt(station: Station): Set[Train] =
    // Could also be expressed in short notation: trains filter (_.stations contains station)
    trains filter (train => train.stations contains station)

  def stopsAt(station: Station): Set[(Time, Train)] =
    for {
      train <- trains
      (time, `station`) <- train.schedule
    } yield (time, train)

  def isShortTrip(from: Station, to: Station): Boolean =
    trains exists (train =>
      train.stations dropWhile (station => station != from) match {
        case `from` +: `to` +: _      => true
        case `from` +: _ +: `to` +: _ => true
        case _                        => false
      }
    )

  def departingHops(station: Station): Map[Station, Set[Hop]] = {
    val trainsAtStation = trainsAt(station)
    val hops = for {
      train <- trainsAtStation
      backToBack <- train.backToBackStations if backToBack._1 == station
    } yield Hop(backToBack._1, backToBack._2, train)
    Map(station -> hops)
  }

  def departingHopsAtTime(departingStation: Station, departingTime: Time): Set[Hop] = {
    for {
      hop <- departingHops(departingStation)(departingStation) if hop.departureTime == departingTime
    } yield hop
  }

  def routes(departureStation: Station, arrivalStation: Station, departureTime: Time, seenStations: Set[Station] = Set()): Set[List[Hop]] = {
    val departingHops: Set[Hop] = departingHopsAtTime(departureStation, departureTime)

    for {
      hop <- departingHops if !(seenStations contains hop.to)
      route <- if (hop.to != arrivalStation) routes(hop.to, arrivalStation, hop.arrivalTime, seenStations + departureStation) else Set(List())
    } yield hop +: route
  }

}
