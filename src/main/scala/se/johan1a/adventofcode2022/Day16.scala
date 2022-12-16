package se.johan1a.adventofcode2022

import scala.collection.mutable.Queue

object Day16 {

  case class Valve(
      name: String,
      rate: Int,
      var next: Seq[(String, Int)],
      open: Boolean
  )

  var i = 0

  case class State(
      valves: Seq[Boolean],
      current: String,
      total: Int,
      flow: Int,
      minutesLeft: Int
  )

  var cache = Map[State, (Int, Seq[String])]()

  def part1(input: Seq[String]): Int = {
    cache = Map()
    i = 0
    val valves: Map[String, Valve] = reduceGraph(
      input.map(parse).map { v => v.name -> v }.toMap
    )
    val start = "AA"
    println(s"reduced valves:")
    valves.foreach(println)
    val maxPossibleFlow = valves.map(_._2.rate).sum
    val (value, path) = findBest(valves, maxPossibleFlow, start, 0, 0, 30)
    println(path)
    value
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

  private def findBest(
      valves: Map[String, Valve],
      maxPossibleFlow: Int,
      current: String,
      total: Int,
      flow: Int,
      minutesLeft: Int
  ): (Int, Seq[String]) = {

    val state = State(valves.values.map(_.open).toSeq, current, total, flow, minutesLeft)
    val cached = cache.get(state)
    cached match {
      case Some(value) => value
      case None =>
        val result: (Int, Seq[String]) = if (minutesLeft == 0) {
          (total, Seq(current))
        } else if (flow == maxPossibleFlow) {
          (total + flow * minutesLeft, Seq(current))
        } else {
          i += 1
          if (i % 100000 == 0) {
            println(i)
          }
          if (i > 20000000) {
            throw new Exception(s"Too many iters, minutes left: $minutesLeft")
          }
          val valve = valves(current)
          val opened = valve.copy(open = true)
          lazy val openResult = findBest(
            valves + (current -> opened),
            maxPossibleFlow,
            current,
            total + flow,
            flow + valve.rate,
            minutesLeft - 1
          )

          lazy val moveResults =
            valve.next.filter(_._2 <= minutesLeft).map { case (next, dist) =>
              findBest(
                valves,
                maxPossibleFlow,
                next,
                total + flow * dist,
                flow,
                minutesLeft - dist
              )
            }
          if (valve.rate > 0 && !valve.open) {
            val rr = (openResult +: moveResults)
            // if(current == "AA") {
            //   println(s"at current: $current. possible:")
            //   rr.foreach(println)
            // }
            val (v, p) = rr.maxBy(_._1)
            (v, (current +: p))
          } else {
            if (moveResults.nonEmpty) {
              val rr = moveResults
              // if (current == "AA" && total > 900) {
              //   println(s"at current: $current. possible:")
              //   rr.foreach(println)
              // }
              val (v, p) = rr.maxBy(_._1)
              (v, (current +: p))
            } else {
              (total + flow * minutesLeft, Seq(current))
            }
          }
        }

        cache = cache + (state -> result)
        result
    }

  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  private def parse(line: String) = {
    line match {
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $valves" =>
        val nextValves = valves.split(", ").map(v => (v, 1))
        Valve(name, rate.toInt, nextValves, false)
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $valve" =>
        Valve(name, rate.toInt, Seq((valve, 1)), false)
    }

  }
}
