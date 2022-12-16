package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.PriorityQueue
import scala.math.Ordering

object Day15 {

  def part1(input: Seq[String], targetY: Int): Int = {
    val sensorsAndBeacons: Seq[(Vec2, Vec2)] = input.map(parse).sortBy(_._1.y)
    val allBeacons = sensorsAndBeacons.map(_._2).toSet
    val allSensors = sensorsAndBeacons.map(_._1).toSet
    val sensorManhattan: Map[Vec2, Long] = sensorsAndBeacons.map {
      case (sensor, beacon) =>
        sensor -> (manhattan(sensor, beacon))
    }.toMap

    var sum = 0

    val minX = sensorsAndBeacons
      .map((pair: (Vec2, Vec2)) => {
        val dist = sensorManhattan(pair._1)
        pair._1.x - dist
      })
      .sorted
      .head

    val maxX = sensorsAndBeacons
      .map((pair: (Vec2, Vec2)) => {
        val dist = sensorManhattan(pair._1)
        pair._1.x + dist
      })
      .sorted
      .last

    minX
      .to(maxX)
      .map(x => {
        val pos = Vec2(x, targetY)
        sensorsAndBeacons.find {
          case (sensor, beacon) => {
            !allBeacons.contains(pos) && manhattan(
              pos,
              sensor
            ) <= sensorManhattan(sensor)
          }
        } match {
          case Some(_) => sum += 1
          case None    =>
        }
      })

    sum
  }

  def part2(input: Seq[String], maxX: Int, maxY: Int): Long = {
    val sensorsAndBeacons: Seq[(Vec2, Vec2)] = input.map(parse).sortBy(_._1.y)
    val (upLines, downLines) = getLines(sensorsAndBeacons)
    println(sensorsAndBeacons)

    val allBeacons = sensorsAndBeacons.map(_._2).toSet
    val allSensors = sensorsAndBeacons.map(_._1).toSet
    val sensorManhattan: Map[Vec2, Long] = sensorsAndBeacons.map {
      case (sensor, beacon) =>
        sensor -> (manhattan(sensor, beacon))
    }.toMap

    val minX = 0
    val minY = 0
    val min = Vec2(0, 0)
    val max = Vec2(maxX, maxY)

    upLines.foreach { up =>
      downLines.foreach { down =>
        val intersection = getIntersection(up, down)
        println(s"up: $up, down: $down, intersection: $intersection")
        if (
          inRange(intersection,min,max) &&
          matches(intersection, sensorsAndBeacons, allBeacons, sensorManhattan)
        ) {
          return intersection.x * 4000000 + intersection.y
        }
      }
    }
    -1
  }

  private def getIntersection(up: Long, down: Long): Vec2 = {
    val x = (up - down ) / 2
    val y = (down + up) / 2
    Vec2(x, y)
  }

  private def getLines(sensorsAndBeacons: Seq[(Vec2, Vec2)]) = {
    val upLines = sensorsAndBeacons.flatMap { case (sensor, beacon) =>
      val dist = manhattan(sensor, beacon)
      val topY = sensor.y - dist - 1
      val topX = sensor.x
      val topUpLine = topY + topX

      val bottomY = sensor.y + dist + 1
      val bottomX = sensor.x
      val bottomUpLine = bottomY + bottomX

      Seq(topUpLine, bottomUpLine)
    }

    val downLines = sensorsAndBeacons.flatMap { case (sensor, beacon) =>
      val dist = manhattan(sensor, beacon)
      val topY = sensor.y - dist - 1
      val topX = sensor.x
      val topDownLine = topY - topX

      val bottomY = sensor.y + dist + 1
      val bottomX = sensor.x
      val bottomDownLine = bottomY - bottomX

      Seq(topDownLine, bottomDownLine)
    }

    (upLines, downLines)
  }

  private def matches(
      pos: Vec2,
      sensorsAndBeacons: Seq[(Vec2, Vec2)],
      allBeacons: Set[Vec2],
      sensorManhattan: Map[Vec2, Long]
  ) = {
    !allBeacons.contains(pos) && sensorsAndBeacons.forall {
      case (sensor, beacon) => {
        manhattan(pos, sensor) > sensorManhattan(sensor)
      }
    }
  }

  private def parse(line: String): (Vec2, Vec2) = {
    val nn = numbers(line)
    val sensor = Vec2(nn.head, nn(1))
    val beacon = Vec2(nn(2), nn(3))
    (sensor, beacon)
  }
}
