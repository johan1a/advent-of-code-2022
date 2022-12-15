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

    var sum = 0

    val minX = 0
    val minY = 0

    println(s"minx: $minX, maxX: $maxX")

    val possibleYs = minY.to(maxY).filter(y => {
      sensorsAndBeacons.exists { case (sensor, beacon) =>
        (y - sensor.y).abs > (beacon.y - sensor.y).abs
      }
    })//.sorted
    val possibleXs = minX.to(maxX).filter(x => {

      val r = sensorsAndBeacons.map { case (sensor, beacon) =>
        val res = (x - sensor.x).abs > (beacon.x - sensor.x).abs
        (sensor, res)
      }

      r.exists(_._2)
    })//.sorted

    println(possibleXs.size * possibleYs.size)
    println(maxX * maxY)

    possibleYs.map(y => {
      possibleXs.map(x => {

      val pos = Vec2(x,y)
      if(sensorsAndBeacons.forall {
        case (sensor, beacon) => {
          !allBeacons.contains(pos) && manhattan(
            pos,
            sensor
          ) > sensorManhattan(sensor)
        }
      }) {
        return pos._1 * 4000000 + pos._2
      }
      })
    })

    // val distressPos = possibleXs.zip(possibleYs).find { case ((x,y)) => {
    //   val pos = Vec2(x,y)
    //   sensorsAndBeacons.forall {
    //     case (sensor, beacon) => {
    //       !allBeacons.contains(pos) && manhattan(
    //         pos,
    //         sensor
    //       ) > sensorManhattan(sensor)
    //     }
    //   }
    // }}.get
    // distressPos._1 * 4000000 + distressPos._2
    -1
  }

  private def parse(line: String): (Vec2, Vec2) = {
    val nn = numbers(line)
    val sensor = Vec2(nn.head, nn(1))
    val beacon = Vec2(nn(2), nn(3))
    (sensor, beacon)
  }
}
