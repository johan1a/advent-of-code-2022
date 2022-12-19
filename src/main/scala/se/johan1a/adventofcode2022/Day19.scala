package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.PriorityQueue

object Day19 {

  case class Robot(name: String, costs: Map[String, Int])
  case class State(
      robots: Map[String, Int],
      resources: Map[String, Int],
      minutesLeft: Int
  )

  def part1(input: Seq[String]): Int = {
    val blueprints = input.map(parse)
    val robots = Map[String, Int]("ore" -> 1)
    blueprints.map { case (id, robotBlueprints) =>
      println(robots)
      val (best, state) = findBest(robotBlueprints, State(robots, Map(), 24))
      println(
        s"id: $id, best: $best, robots: ${state.robots}, resources: ${state.resources}"
      )
      best * id
    }.sum
  }

  private def score(state: State) = {
    100 * get(state.resources, "geode")
      + 100 * get(state.robots, "geode") * state.minutesLeft

      + 20 * get(state.resources, "obsidian")
      + 20 * get(state.robots, "obsidian") * state.minutesLeft

      + 10 * get(state.resources, "clay")
      + 10 * get(state.robots, "clay") * state.minutesLeft

      + 10 * get(state.resources, "ore")
      + 10 * get(state.robots, "ore") * state.minutesLeft
  }

  private def get(m: Map[String, Int], k: String): Int = {
    m.getOrElse(k, 0)
  }

  private def findBest(
    blueprints: Seq[Robot],
      start: State
  ): (Int, State) = {
    var i = 0

    var queue = PriorityQueue[State]()(Ordering.by { s =>
      -score(s)
    })

    var seen = Set[State]()

    queue.enqueue(start)
    var best = 0
    var bestState = start
    val maxQueueDepth = 5000

    while (queue.nonEmpty) {
      var state = queue.dequeue()
      while (seen.contains(state)) {
        state = queue.dequeue()
      }

      // assert(i < 900000)
      if (i % 10000 == 0) {
        println(
          s"i: $i, best: $best, current state: $state, score: ${score(state)}, queue.size: ${queue.size}"
        )
      }
      i += 1

      val nbrGeodes = state.resources.getOrElse("geode", 0)
      val nbrGeodeRobots = state.robots.getOrElse("geode", 0)

      if (state.minutesLeft == 0) {
        best = Math.max(best, (state.resources.getOrElse("geode", 0)))
        bestState = state
      } else if (
        nbrGeodes + nbrGeodeRobots * state.minutesLeft + sum(
          state.minutesLeft - 1
        ) < best
      ) {
        // Todo continue
        // todo else if (hasMaxRobots(blueprints, robots)) { continue
      } else {

        val newResources = state.robots.map { case ((resource, nbr)) =>
          (resource -> nbr)
        }

        var r1: Seq[State] = pickBest(
          blueprints,
          blueprints
            .filter { blueprint =>
              blueprint.costs.forall { case (resource, needed) =>
                val amount = state.resources.getOrElse(resource, 0)
                amount >= needed
              }
            },
          state.robots,
          state.minutesLeft
        ).map { blueprint =>
          val resourcesNeeded = blueprint.costs.map { case (resource, amount) =>
            resource -> amount.toInt
          }.toMap

          var resourcesAfterPurchase = sub(state.resources, resourcesNeeded)
          val newRobots: Map[String, Int] = state.robots.updated(
            blueprint.name,
            state.robots.getOrElse(blueprint.name, 0) + 1
          )

          State(
            newRobots,
            add(resourcesAfterPurchase, newResources),
            state.minutesLeft - 1
          )
        }
        r1.foreach { s =>
          queue.enqueue(s)
        }

        queue.enqueue(
          State(
            state.robots,
            add(state.resources, newResources),
            state.minutesLeft - 1
          )
        )
      }

      if (queue.size > maxQueueDepth) {
        queue = queue.take(maxQueueDepth)
      }
    }

    (best, bestState)
  }

  var sumCache = Map[Int, Int](0 -> 0)

  private def sum(minutesLeft: Int): Int = {
    if (minutesLeft == 0) {
      0
    } else if (sumCache.contains(minutesLeft)) {
      sumCache(minutesLeft)
    } else {
      val res = minutesLeft + sum(minutesLeft - 1)
      sumCache = sumCache + (minutesLeft -> res)
      res

    }
  }

  private def pickBest(
      blueprints: Seq[Robot],
      affordableBlueprints: Seq[Robot],
      robots: Map[String, Int],
      minutesLeft: Int
  ): Seq[Robot] = {
    val nbrObsidian = robots.getOrElse("obsidian", 0)
    val nbrClay = robots.getOrElse("clay", 0)
    val nbrOre = robots.getOrElse("ore", 0)

    var maxObsidian =
      blueprints.map(_.costs.getOrElse("obsidian", 0)).maxOption.getOrElse(0)
    var maxClay =
      blueprints.map(_.costs.getOrElse("clay", 0)).maxOption.getOrElse(0)
    var maxOre = blueprints
      .map(_.costs.getOrElse("ore", 0))
      .maxOption
      .getOrElse(0)

    // println(
    //   s"obsidian: $nbrObsidian / $maxObsidian, clay: $nbrClay / $maxClay, ore: $nbrOre / $maxOre"
    // )

    var result = affordableBlueprints
    if (nbrObsidian >= maxObsidian) {
      result = result.filterNot(_.name == "obsidian")
    }
    if (nbrClay >= maxClay) {
      result = result.filterNot(_.name == "clay")
    }
    if (nbrOre >= maxOre) {
      result = result.filterNot(_.name == "ore")
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

  def part2(input: Seq[String]): Int = {
    -1
  }

  private def parse(line: String): (Int, Seq[Robot]) = {
    var split = line.split(":")
    val id = split.head.split("Blueprint ").last.toInt
    var robots = split.last
    (
      id,
      robots
        .split("""\.""")
        .map {
          case s" Each $name robot costs $a $aOre and $b $bOre" =>
            Robot(name, Map(aOre -> a.toInt, bOre -> b.toInt))
          case s" Each $name robot costs $a $aOre" =>
            Robot(name, Map(aOre -> a.toInt))
        }
        .toSeq
    )
  }

  private def hasMaxRobots(blueprints: Seq[Robot], robots: Map[String, Int]) = {
    val nbrObsidian = robots.getOrElse("obsidian", 0)
    val nbrClay = robots.getOrElse("clay", 0)
    val nbrOre = robots.getOrElse("ore", 0)

    val maxObsidian =
      blueprints.map(_.costs.getOrElse("obsidian", 0)).maxOption.getOrElse(0)
    val maxClay =
      blueprints.map(_.costs.getOrElse("clay", 0)).maxOption.getOrElse(0)
    val maxOre = blueprints
      .map(_.costs.getOrElse("ore", 0))
      .maxOption
      .getOrElse(0)
    nbrObsidian == maxObsidian && nbrClay == maxClay && nbrOre == maxOre
  }

}
