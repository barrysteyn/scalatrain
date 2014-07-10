/*
 * Copyright © 2012 Typesafe, Inc. All rights reserved.
 */

package com.typesafe.training.scalatrain

case class Hop(from: Station, to: Station, train: Train) {
  val departureTime = train departureTime from
  val arrivalTime = train departureTime to
}

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
  val departingHops : Map[Station, Set[Hop]] = {
    	val hops : Set[Hop] = for {
    	  train <- trains
    	  backToBack <- train.backToBackStations 
    	} yield Hop(backToBack._1, backToBack._2, train)
    	
    	hops.groupBy (hop => hop.from)
  }

  def departingHopsAtTime(departingStation: Station, departingTime: Time): Set[Hop] =
     departingHops(departingStation) filter (hop => hop.departureTime >= departingTime)

  def routes(departureStation: Station, arrivalStation: Station, departureTime: Time, seenStations: Set[Station] = Set()): Set[List[Hop]] = {
    val departingHops: Set[Hop] = departingHopsAtTime(departureStation, departureTime)

    for {
      hop 	<- 	departingHops if !(seenStations contains hop.to)
      route <- 	if (hop.to != arrivalStation) 
    	  			routes(hop.to, arrivalStation, hop.arrivalTime, seenStations + departureStation) 
    	  		else 
    	  			Set(List())
    } yield hop +: route
  }

}
