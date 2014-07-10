/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain
import scala.collection.mutable.TreeSet

case class Hop(from: Station, to: Station, train: Train, cost : Int = 0) {
  val departureTime = train departureTime from
  val arrivalTime = train departureTime to
  
  override def toString : String = "($from -> $to)"
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

 // def trainSchedule(date : java.util.Date) : Set[Train]
    
    
  //A mapping from Station to hops
  val departingHops: Map[Station, Set[Hop]] = {
    val hops: Set[Hop] = for {
      train <- trains
      backToBack <- train.backToBackStations
    } yield Hop(backToBack._1, backToBack._2, train)

    hops groupBy(_.from)
  }

  def departingHopsAtTime(departingStation: Station, departingTime: Time): Set[Hop] =
    departingHops(departingStation) filter (hop => hop.departureTime >= departingTime)

  //Sort paths in ascending order via time
  def sortPathsByTime(paths: Set[List[Hop]]) = 
  	paths.toVector.sortBy(hop => hop.last.arrivalTime - hop.head.departureTime)

  //Sort paths in ascending order via cost
  def sortPathsByCost(paths: Set[List[Hop]]) = {
    	//Creates a list of tuples of, each tuple being (cost, List[Hop])
    	val costPath = for {
    	  path <- paths.toList
    	  cost = path.foldLeft(0)(_ + _.cost)
    	} yield (cost, path)
    	
    	//Returns a vector of List[Hop], sorted by total cost
    	for {
    	  cp <- costPath.groupBy(_._1).toVector.sortBy(_._1)
    	} yield cp._2
  }
  
  //Get paths between two stations given a departure time
  def getPathsAtTime(departureStation: Station, arrivalStation: Station, departureTime: Time): Set[List[Hop]] =
    routes(departureStation, arrivalStation, departureTime)

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
