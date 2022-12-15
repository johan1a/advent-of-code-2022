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

    // find corners and iterate line by line

    // find s with miny,minx,maxy,maxy (4 poins)
    // for each s
    //    find dist to nearest b
    //    if s has miny
    //      global_min_y <- add y - manhattan(b)
    //      ...
    //
    // traverse each point , line by line from miny to maxy, minx to maxx
    //    for earch s
    //      if dist between point to s <= dist between point to its nearest b
    //        sum +=1
    //        break
    //
    // return sum

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
    val sensorsAndBeacons: Seq[((Int, Int), (Int, Int))] = input.map(parse).sortBy(_._1.y).map{ case ((s,e)) =>
      ((s.x.toInt,s.y.toInt), (e.x.toInt,e.y.toInt))
    }
    val allBeacons = sensorsAndBeacons.map(e => (e._2._1, e._2._2)).toSet
    val sensorManhattan: Map[(Int, Int), Int] = sensorsAndBeacons.map {
      case (sensor, beacon) =>
        (sensor._1.toInt, sensor._2.toInt) -> (manhattan(sensor, beacon).toInt)
    }.toMap

    var sum = 0

    // val minX = sensorsAndBeacons.map((pair: (Vec2, Vec2)) => {
    //   val dist = sensorManhattan(pair._1)
    //   pair._1.x - dist
    // }).sorted.head

    // val maxX = sensorsAndBeacons.map((pair: (Vec2, Vec2)) => {
    //   val dist = sensorManhattan(pair._1)
    //   pair._1.x + dist
    // }).sorted.last

    // val minY = sensorsAndBeacons.map((pair: (Vec2, Vec2)) => {
    //   val dist = sensorManhattan(pair._1)
    //   pair._1.y - dist
    // }).sorted.head

    // val maxY = sensorsAndBeacons.map((pair: (Vec2, Vec2)) => {
    //   val dist = sensorManhattan(pair._1)
    //   pair._1.y + dist
    // }).sorted.last
    val minX = 0
    val minY = 40

    println(s"minx: $minX, maxX: $maxX, minY: $minY, maxY: $maxY")

    minY
      .to(maxY)
      .map(y => {
        minX
          .to(maxX)
          .map(x => {
            val pos = (x, y)
            if (y % 10 == 0 && x % 1000000 == 0) {
              println(s"x: $x, y: $y. ${y / maxY.toDouble * 100}%")
            }
            if (
              sensorsAndBeacons.exists {
                case (sensor, beacon) => {
                  !allBeacons.contains(pos) && manhattan(
                    pos,
                    sensor
                  ) <= sensorManhattan(sensor)
                }
              }
            ) {
              // continue
            } else {
              println(s"pos: $pos")
              return pos._1 * 4000000 + pos._2
            }
          })
      })

    -1
  }

  private def parse(line: String): (Vec2, Vec2) = {
    val nn = numbers(line)
    val sensor = Vec2(nn.head, nn(1))
    val beacon = Vec2(nn(2), nn(3))
    (sensor, beacon)
  }
}
