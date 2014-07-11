/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain
import scala.collection.mutable.TreeSet

sealed case class Path(hops: List[Hop]) {
  val totalCost = hops.foldLeft(0)((acc: Int, hop: Hop) => acc + hop.cost)
  val train: Train = hops.head.train
  val timeTable: Seq[(Time, Station)] = train.schedule.timeTable
}

case class Hop(from: Station, to: Station, train: Train, cost: Int = 0) {
  val departureTime = train departureTime from
  val arrivalTime = train departureTime to

  override def toString: String = "($from -> $to)"
}

//case class Path(pathList)

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
      (time, `station`) <- train.schedule.timeTable
    } yield (time, train)

  //Tests whether the trip is considered short  
  def isShortTrip(from: Station, to: Station): Boolean =
    trains exists (train =>
      train.stations dropWhile (station => station != from) match {
        case `from` +: `to` +: _      => true
        case `from` +: _ +: `to` +: _ => true
        case _                        => false
      }
    )

  //A mapping from Station to hops
  val departingHops: Map[Station, Set[Hop]] = {
    val hops: Set[Hop] = for {
      train <- trains
      backToBack <- train.backToBackStations
    } yield Hop(backToBack._1, backToBack._2, train)

    hops groupBy (_.from)
  }

  def departingHopsAtTime(departingStation: Station, departingTime: Time): Set[Hop] =
    departingHops(departingStation) filter (hop => hop.departureTime >= departingTime)

  //Sort paths in ascending order via time
  def sortPathsByTime(paths: Set[Path]): List[Path] =
    paths.toList.sortBy(path => path.hops.last.arrivalTime - path.hops.head.departureTime)

  def sortPathsByCost(paths: Set[Path]): List[Path] =
    paths.toList.sortBy(path => path.totalCost)

  //Get paths between two stations given a departure time
  def getPathsAtTime(departureStation: Station, arrivalStation: Station, departureTime: Time): Set[Path] =
    routes(departureStation, arrivalStation, departureTime) map (listHop => Path(listHop))

  private def routes(departureStation: Station, arrivalStation: Station, departureTime: Time, seenStations: Set[Station] = Set()): Set[List[Hop]] = {
    val departingHops: Set[Hop] = departingHopsAtTime(departureStation, departureTime)

    for {
      hop <- departingHops if !(seenStations contains hop.to)
      route <- if (hop.to != arrivalStation)
        routes(hop.to, arrivalStation, hop.arrivalTime, seenStations + departureStation)
      else
        Set(List())
    } yield hop +: route
  }

}
