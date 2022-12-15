package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.Queue

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

    println(s"minx: $minX, maxX: $maxX")

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
    val allBeacons = sensorsAndBeacons.map(_._2).toSet
    val allSensors = sensorsAndBeacons.map(_._1).toSet
    val sensorManhattan: Map[Vec2, Long] = sensorsAndBeacons.map {
      case (sensor, beacon) =>
        sensor -> (manhattan(sensor, beacon))
    }.toMap


    val min = Vec2(0, 0)
    val max = Vec2(maxX, maxY)

    println("calculate edges")
    val possible = sensorsAndBeacons
      .map(sb => {
        println(s"calculating edges for sensor: ${sb._1}")
        //val ee = edges(sb._1, sb._2).filter(p => inRange(p, min, max))
        val sensor = sb._1
        val beacon = sb._2

        val dist = manhattan(sensor, beacon) + 1

        var positions = Seq[Vec2]()

        val above = add(sensor, Vec2(0, -dist))
        val below = add(sensor, Vec2(0, dist))
        val right = add(sensor, Vec2(dist, 0))
        val left = add(sensor, Vec2(-dist, 0))

        var i = 0
        val period = 10000

        println("At above")
        var pos = above
        while (pos != right) {
          positions = positions :+ pos
          pos = add(pos, Vec2(1, 1))

          if(i % period == 0 ){
            println(s"sensor: $sensor, pos: $pos, i: $i")
          }
          i += 1

          if (inRange(pos, min, max) && sensorsAndBeacons.forall { case ((sensor, beacon)) =>
            manhattan(pos, sensor) > sensorManhattan(sensor)
          }) {
            return pos.x * 4000000 + pos.y
          }
        }
        println("At right")
        while (pos != below) {
          positions = positions :+ pos
          pos = add(pos, Vec2(-1, 1))
          if(i % period == 0 ){
            println(s"sensor: $sensor, pos: $pos, i: $i")
          }
          i += 1
          if (inRange(pos, min, max) && sensorsAndBeacons.forall { case ((sensor, beacon)) =>
            manhattan(pos, sensor) > sensorManhattan(sensor)
          }) {
            return pos.x * 4000000 + pos.y
          }
        }
        println("At below")
        while (pos != left) {
          positions = positions :+ pos
          pos = add(pos, Vec2(-1, -1))
          if(i % period == 0 ){
            println(s"sensor: $sensor, pos: $pos, i: $i")
          }
          i += 1
          if (inRange(pos, min, max) && sensorsAndBeacons.forall { case ((sensor, beacon)) =>
            manhattan(pos, sensor) > sensorManhattan(sensor)
          }) {
            return pos.x * 4000000 + pos.y
          }
        }
        println("At left")
        while (pos != above) {
          positions = positions :+ pos
          pos = add(pos, Vec2(1, -1))
          if(i % period == 0 ){
            println(s"sensor: $sensor, pos: $pos, i: $i")
          }
          i += 1
          if (inRange(pos, min, max) && sensorsAndBeacons.forall { case ((sensor, beacon)) =>
            manhattan(pos, sensor) > sensorManhattan(sensor)
          }) {
            return pos.x * 4000000 + pos.y
          }
        }
        -1
      })
    -1
  }

  private def edges(sensor: Vec2, beacon: Vec2): Seq[Vec2] = {
    val dist = manhattan(sensor, beacon) + 1

    var positions = Seq[Vec2]()

    val above = add(sensor, Vec2(0, -dist))
    val below = add(sensor, Vec2(0, dist))
    val right = add(sensor, Vec2(dist, 0))
    val left = add(sensor, Vec2(-dist, 0))

    var pos = above
    while (pos != right) {
      positions = positions :+ pos
      pos = add(pos, Vec2(1, 1))
    }
    while (pos != below) {
      positions = positions :+ pos
      pos = add(pos, Vec2(-1, 1))
    }
    while (pos != left) {
      positions = positions :+ pos
      pos = add(pos, Vec2(-1, -1))
    }
    while (pos != above) {
      positions = positions :+ pos
      pos = add(pos, Vec2(1, -1))
    }

    positions
  }

  private def parse(line: String): (Vec2, Vec2) = {
    val nn = numbers(line)
    val sensor = Vec2(nn.head, nn(1))
    val beacon = Vec2(nn(2), nn(3))
    (sensor, beacon)
  }
}
