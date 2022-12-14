package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.PriorityQueue

object Day19 {

  private val ore = "ore"
  private val clay = "clay"
  private val obsidian = "obsidian"
  private val geode = "geode"

  case class Blueprint(robotType: String, costs: Map[String, Int])

  case class State(
      robots: Map[String, Int],
      resources: Map[String, Int],
      minutesLeft: Int
  )

  def part1(input: Seq[String]): Int = {
    val blueprints = input.map(parse)
    val robots = Map[String, Int](ore -> 1)
    blueprints.map { case (id, robotBlueprints) =>
      val (best, state) =
        findBest(robotBlueprints, State(robots, Map(), 24), 5000)
      best * id
    }.sum
  }

  def part2(input: Seq[String]): Int = {
    val blueprints = input.take(3).map(parse)
    val robots = Map[String, Int](ore -> 1)
    blueprints.map { case (id, robotBlueprints) =>
      val (best, _) =
        findBest(robotBlueprints, State(robots, Map(), 32), 20000)
      best
    }.product
  }

  private def findBest(
      blueprints: Seq[Blueprint],
      start: State,
      maxQueueDepth: Int
  ): (Int, State) = {
    var i = 0

    val maxObsidian =
      blueprints.map(_.costs.getOrElse(obsidian, 0)).maxOption.getOrElse(0)
    val maxClay =
      blueprints.map(_.costs.getOrElse(clay, 0)).maxOption.getOrElse(0)
    val maxOre = blueprints
      .map(_.costs.getOrElse(ore, 0))
      .maxOption
      .getOrElse(0)

    var seen = Set[State]()
    var queue = PriorityQueue[State]()(Ordering.by { s =>
      score(s)
    })

    queue.enqueue(start)
    var best = 0
    var bestState = start

    while (queue.nonEmpty) {
      var state = queue.dequeue()
      while (queue.nonEmpty && seen.contains(state)) {
        state = queue.dequeue()
      }
      seen = seen + state

      val nbrGeode = state.resources.getOrElse(geode, 0)
      val nbrGeodeRobots = state.robots.getOrElse(geode, 0)
      best = Math.max(best, nbrGeode + nbrGeodeRobots * state.minutesLeft)

      assert(i < 1000000)
      i += 1

      if (state.minutesLeft == 0) {
        best = Math.max(best, (state.resources.getOrElse(geode, 0)))
        bestState = state
      } else if (
        nbrGeode + nbrGeodeRobots * state.minutesLeft + sum(
          state.minutesLeft - 1
        ) >= best
      ) {
        val newStates: Seq[State] = filterBlueprints(
          maxObsidian,
          maxClay,
          maxOre,
          blueprints.filter(b => hasRobotsFor(state.robots, b)),
          state.robots,
          state.minutesLeft
        ).map { blueprint =>
          var waitTime = 1
          var newResources = state.resources
          val producedResources = state.robots
          // wait until we can afford this robot
          while (
            !canAfford(
              newResources,
              blueprint
            ) && state.minutesLeft - waitTime > 0
          ) {
            newResources = add(newResources, producedResources)
            waitTime += 1
          }
          // always takes at least 1 minute
          newResources = add(newResources, producedResources)

          var newRobots = state.robots
          if (canAfford(newResources, blueprint)) {
            newResources = sub(newResources, blueprint.costs)

            newRobots = state.robots.updated(
              blueprint.robotType,
              state.robots.getOrElse(blueprint.robotType, 0) + 1
            )
          }

          val newMinutesLeft = state.minutesLeft - waitTime

          State(
            newRobots,
            newResources,
            newMinutesLeft
          )
        }
        newStates.foreach { s =>
          queue.enqueue(s)
        }
      }

      if (queue.size > maxQueueDepth) {
        queue = queue.take(maxQueueDepth)
      }
    }

    (best, bestState)
  }

  private def score(state: State) = {
    (
      -get(state.robots, ore),
      -get(state.robots, clay),
      -get(state.robots, obsidian),
      -get(state.robots, geode)
    )
  }

  private def get(m: Map[String, Int], k: String): Int = {
    m.getOrElse(k, 0)
  }

  private def canAfford(resources: Map[String, Int], blueprint: Blueprint) = {
    blueprint.costs.forall { case (resource, needed) =>
      val amount = resources.getOrElse(resource, 0)
      amount >= needed
    }
  }

  private def hasRobotsFor(robots: Map[String, Int], blueprint: Blueprint) = {
    blueprint.costs.forall { (name, _) =>
      get(robots, name) > 0
    }
  }

  private var sumCache = Map[Int, Int](0 -> 0)

  private def sum(minutesLeft: Int): Int = {
    if (minutesLeft <= 0) {
      0
    } else if (sumCache.contains(minutesLeft)) {
      sumCache(minutesLeft)
    } else {
      val res = minutesLeft + sum(minutesLeft - 1)
      sumCache = sumCache + (minutesLeft -> res)
      res
    }
  }

  private def filterBlueprints(
      maxObsidian: Int,
      maxClay: Int,
      maxOre: Int,
      affordableBlueprints: Seq[Blueprint],
      robots: Map[String, Int],
      minutesLeft: Int
  ): Seq[Blueprint] = {
    val nbrObsidian = robots.getOrElse(obsidian, 0)
    val nbrClay = robots.getOrElse(clay, 0)
    val nbrOre = robots.getOrElse(ore, 0)

    var result = affordableBlueprints
    if (minutesLeft <= 4 || nbrObsidian >= maxObsidian) {
      result = result.filterNot(_.robotType == obsidian)
    }
    if (minutesLeft <= 7 || nbrClay >= maxClay) {
      result = result.filterNot(_.robotType == clay)
    }
    if (minutesLeft <= 17 || nbrOre >= maxOre) {
      result = result.filterNot(_.robotType == ore)
    }
    result
  }

  private def add(
      a: Map[String, Int],
      b: Map[String, Int]
  ): Map[String, Int] = {
    a ++ b.map { case (k, v) => k -> (a.getOrElse(k, 0) + v) }
  }

  private def sub(
      a: Map[String, Int],
      b: Map[String, Int]
  ): Map[String, Int] = {
    a ++ b.map { case (k, v) => k -> (a.getOrElse(k, 0) - v) }
  }

  private def parse(line: String): (Int, Seq[Blueprint]) = {
    val split = line.split(":")
    val id = split.head.split("Blueprint ").last.toInt
    val robots = split.last
    (
      id,
      robots
        .split("""\.""")
        .map {
          case s" Each $name robot costs $a $aOre and $b $bOre" =>
            Blueprint(name, Map(aOre -> a.toInt, bOre -> b.toInt))
          case s" Each $name robot costs $a $aOre" =>
            Blueprint(name, Map(aOre -> a.toInt))
        }
        .toSeq
    )
  }

}
