package se.johan1a.adventofcode2022

import scala.collection.mutable.Queue
import scala.collection.mutable

object Day16 {

  case class Valve(
      name: String,
      rate: Int,
      var next: Seq[(String, Int)],
      open: Boolean
  )
  var valves = Map[String, Valve]()

  def part1(input: Seq[String]): Int = {
    val (valves0, dist0) = reduceGraph2(
      input.map(parse).map { v => v.name -> v }.toMap
    )
    dist = dist0
    valves2 = valves0

    val start = "AA"

    val leftToVisit = valves2.map(_._1).filterNot(_ == start).toSeq

    val results = findBest2(leftToVisit, start, 30)
    val rr = results.sortBy(_._1)

    rr.last._1
  }

  private def reduceGraph(valves: Map[String, Valve]): Map[String, Valve] = {
    var reducedValves = valves

    valves.values.foreach { valve0 =>
      if (valve0.rate == 0 && valve0.name != "AA") {
        val neighbors = valve0.next
        valves.values.foreach { valve1 =>
          valve1.next.find(_._1 == valve0.name) match {
            case Some((n0, d0)) =>
              valve1.next =
                valve1.next.filterNot(_._1 == valve0.name) ++ neighbors
                  .map { case (n1, d1) =>
                    (n1, d1 + d0)
                  }
                  .filterNot(_._1 == valve1.name)
            case None =>
          }
        }

        reducedValves = reducedValves.removed(valve0.name)
      }
    }

    reducedValves
  }

  var valves2 = Map[String, Valve]()
  var dist = mutable.Map[(String, String), Short]()

  private def findBest2(
      leftToVisit: Seq[String],
      current: String,
      minutesLeft: Short
  ): Seq[(Short, Seq[String])] = {

    val currentValve = valves2(current)
    val valveTotal = currentValve.rate * minutesLeft

    val r = leftToVisit
      .filter(next => dist((current, next)) + 1 <= minutesLeft)
      .flatMap { next =>
        val d = dist((current, next))
        val results =
          findBest2(
            leftToVisit.filterNot(_ == next),
            next,
            (minutesLeft - d - 1).toShort
          )

        val r0 = results.map { case (value, path) =>
          ((value + valveTotal).toShort, next +: path)
        }
        r0
      }

    if (r.nonEmpty) {
      r
    } else {
      Seq((valveTotal.toShort, Seq(current)))
    }
  }

  private def reduceGraph2(
      valves: Map[String, Valve]
  ): (Map[String, Valve], mutable.Map[(String, String), Short]) = {
    var reducedValves = valves

    valves.values.foreach { valve0 =>
      if (valve0.rate == 0 && valve0.name != "AA") {
        val neighbors = valve0.next
        valves.values.foreach { valve1 =>
          valve1.next.find(_._1 == valve0.name) match {
            case Some((n0, d0)) =>
              valve1.next =
                valve1.next.filterNot(_._1 == valve0.name) ++ neighbors
                  .map { case (n1, d1) =>
                    (n1, d1 + d0)
                  }
                  .filterNot(_._1 == valve1.name)
            case None =>
          }
        }

        reducedValves = reducedValves.removed(valve0.name)
      }
    }

    var dist =
      mutable.Map[(String, String), Short]().withDefaultValue(10000.toShort)

    reducedValves.values.foreach { valve =>
      dist((valve.name, valve.name)) = 0
      valve.next.foreach { case (neighbor, neighborDist) =>
        dist((valve.name, neighbor)) = neighborDist.toShort
      }
    }

    reducedValves.values.foreach { k =>
      reducedValves.values.foreach { i =>
        reducedValves.values.foreach { j =>
          val potential =
            (dist((i.name, k.name)) + dist((k.name, j.name))).toShort
          if (dist((i.name, j.name)) > potential) {
            dist((i.name, j.name)) = potential
          }
        }
      }
    }

    (reducedValves, dist)
  }

  private def parse(line: String) = {
    line match {
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $valves" =>
        val nextValves = valves.split(", ").map(v => (v, 1))
        Valve(name, rate.toShort, nextValves, false)
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $valve" =>
        Valve(name, rate.toShort, Seq((valve, 1)), false)
    }

  }
}
