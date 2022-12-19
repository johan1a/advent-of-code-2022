package se.johan1a.adventofcode2022

import Utils._

object Day19 {

  case class Robot(name: String, costs: Map[String, Int])
  case class State(
      blueprints: Seq[Robot],
      robots: Map[String, Int],
      resources: Map[String, Int],
      minutesLeft: Int
  )

  var i = 0
  var globalBest = 0

  var cache = Map[State, (Int, State)]()

  def part1(input: Seq[String]): Int = {
    i = 0
    val blueprints = input.map(parse)
    val robots = Map[String, Int]("ore" -> 1)
    blueprints.map { case (id, robotBlueprints) =>
      cache = Map[State, (Int, State)]()
      globalBest = 0

      println(robots)
      val (best, state) = findBest(robotBlueprints, robots, Map(), 24, false)
      println(
        s"id: $id, best: $best, robots: ${state.robots}, resources: ${state.resources}"
      )
      best * id
    }.sum
  }

  private def findBest(
      blueprints: Seq[Robot],
      robots: Map[String, Int],
      resources: Map[String, Int],
      minutesLeft: Int,
      waited: Boolean
  ): (Int, State) = {
    assert(i < 1000000)
    if (i % 1000000 == 0) {
      println(i)
    }
    i += 1

    var state = State(blueprints, robots, resources, minutesLeft)

    if (minutesLeft == 0) {
      (resources.getOrElse("geode", 0), state)
    } else if (cache.contains(state)) {
      cache(state)
    } else if (
      resources.getOrElse("geode", 0) + robots.getOrElse(
        "geode",
        0
      ) * minutesLeft + sum(minutesLeft - 1) < globalBest
    ) {
      cache = cache + (state -> (globalBest, state))
      (globalBest, state)
    } else if (hasMaxRobots(blueprints, robots)) {

      ???
    } else {

      val newResources = robots.map { case ((resource, nbr)) =>
        (resource -> nbr)
      }

      var r1 = pickBest(
        blueprints,
        blueprints
          .filter { blueprint =>
            if (waited) {
              blueprint.costs.forall { case (resource, needed) =>
                val amount = resources.getOrElse(resource, 0) - 1
                amount >= needed
              }
            } else {
              blueprint.costs.forall { case (resource, needed) =>
                val amount = resources.getOrElse(resource, 0)
                amount >= needed
              }
            }
          },
        robots,
        minutesLeft
      )
        .map { blueprint =>
          val resourcesNeeded = blueprint.costs.map { case (resource, amount) =>
            resource -> amount.toInt
          }.toMap
          var resourcesAfterPurchase = sub(resources, resourcesNeeded)
          val newRobots =
            robots.updated(
              blueprint.name,
              robots.getOrElse(blueprint.name, 0) + 1
            )

          findBest(
            blueprints,
            newRobots,
            add(resourcesAfterPurchase, newResources),
            minutesLeft - 1,
            false
          )
        }

      lazy val r0 = findBest(
        blueprints,
        robots,
        add(resources, newResources),
        minutesLeft - 1,
        true
      )

      val maxOre = blueprints
        .map(_.costs.getOrElse("ore", 0))
        .maxOption
        .getOrElse(0)

      r1 = r1 :+ r0

      val res = r1.maxBy(_._1)
      globalBest = Math.max(res._1, globalBest)
      cache = cache + (state -> res)
      res
    }
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
}
