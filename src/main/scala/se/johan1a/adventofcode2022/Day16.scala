package se.johan1a.adventofcode2022

import scala.collection.mutable.Queue

object Day16 {

  case class Valve(
      name: String,
      rate: Int,
      var next: Seq[(String, Int)],
      open: Boolean
  )

  case class State(
      valves: Map[String, Boolean],
      current: String,
      total: Short,
      minutesLeft: Short
  )

  var cache = Map[State, (Short, Seq[String])]()
  var valves = Map[String, Valve]()
  var i = 0

  def part1(input: Seq[String]): Int = {
    cache = Map()
    i = 0
    valves = reduceGraph(
      input.map(parse).map { v => v.name -> v }.toMap
    )
    val start = "AA"
    println(s"reduced valves:")
    valves.foreach(println)
    val maxPossibleFlow = valves.map(_._2.rate).sum.toShort
    val initialStates: Map[String, Boolean] =
      valves.map(e => e._1 -> false).toMap
    val (value, path) =
      findBest(initialStates, maxPossibleFlow, start, 0, 0, 30)
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
      valveStates: Map[String, Boolean],
      maxPossibleFlow: Short,
      current: String,
      total: Short,
      flow: Short,
      minutesLeft: Short
  ): (Short, Seq[String]) = {

    val state = State(valveStates, current, total, minutesLeft)
    val cached = cache.get(state)
    cached match {
      case Some(value) => value
      case None =>
        val result: (Short, Seq[String]) = if (minutesLeft == 0) {
          (total, Seq(current))
        } else if (flow == maxPossibleFlow) {
          (total, Seq(current))
        } else {
          i += 1
          if (i % 100000 == 0) {
            println(i)
          }
          if (i > 20000000) {
            throw new Exception(s"Too many iters, minutes left: $minutesLeft")
          }
          val valve = valves(current)
          lazy val openResult = findBest(
            valveStates + (current -> true),
            maxPossibleFlow,
            current,
            (total + valve.rate * (minutesLeft - 1)).toShort,
            (flow + valve.rate).toShort,
            (minutesLeft - 1).toShort
          )

          lazy val moveResults =
            valve.next.filter(_._2 <= minutesLeft).map { case (next, dist) =>
              findBest(
                valveStates,
                maxPossibleFlow,
                next,
                total,
                flow,
                (minutesLeft - dist).toShort
              )
            }
          if (valve.rate > 0 && !valveStates(current)) {
            val (v, p) = (openResult +: moveResults).maxBy(_._1)
            (v, (current +: p))
          } else if (moveResults.nonEmpty) {
            val (v, p) = moveResults.maxBy(_._1)
            (v, (current +: p))
          } else {
            (total, Seq(current))
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
        Valve(name, rate.toShort, nextValves, false)
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $valve" =>
        Valve(name, rate.toShort, Seq((valve, 1)), false)
    }

  }
}
