package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.Queue

object Day15 {

  def part1(input: Seq[String], targetY: Int): Int = {
    val sensorsAndBeacons = input.map(parse)
    var grid = Map[Vec2, Char]()
    sensorsAndBeacons.foreach {
      case (sensor, nearestBeacon) => {
        println(s"sensor: $sensor, nearestBeacon: $nearestBeacon")
        grid = grid + (sensor -> 'S')
        grid = grid + (nearestBeacon -> 'B')
        var positionsToCheck = Queue(sensor)
        while (positionsToCheck.nonEmpty) {
          val pos = positionsToCheck.dequeue()

          if (manhattan(sensor, pos) <= manhattan(sensor, nearestBeacon)) {
            grid = grid + (pos -> '#')
            positionsToCheck.enqueueAll(neighbors(pos).filterNot(grid.contains))
          }
        }
      }
    }

    grid.filter(entry => entry._1.y == targetY && entry._2 != 'B').size
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  private def parse(line: String): (Vec2, Vec2) = {
    val nn = numbers(line)
    val sensor = Vec2(nn.head, nn(1))
    val beacon = Vec2(nn(2), nn(3))
    (sensor, beacon)
  }
}
