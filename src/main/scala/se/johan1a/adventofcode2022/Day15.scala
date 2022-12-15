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
    println(sensorsAndBeacons)
    return -1

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

    // 0.to(maxY).map { y =>
    //   0.to(maxX).map { x =>
    //     manhattanDiffSum(Vec2(x, y), sensorsAndBeacons, sensorManhattan)
    //   }
    // }.foreach(println)

    println("\nstart search x")
    var y = maxY / 2
    val (startX, startXVal) =
      findStartX(y, minX, maxX, sensorsAndBeacons, sensorManhattan)
    println("start search y")
    val (startY, startYVal) =
      findStartY(startX, minY, maxY, sensorsAndBeacons, sensorManhattan)

    println(
      s"startX: $startX, startXVal: $startXVal, startY: $startY, startYVal: $startYVal"
    )

    var pos = Vec2(startX, startY)
    var seen = Set[Vec2]()
    var queue = PriorityQueue[(Vec2, Long)]((pos, startYVal))(
      Ordering.by(p => -p._2)
    )
    var i = 0


    while (!matches(pos, sensorsAndBeacons, allBeacons, sensorManhattan)) {
      var d = queue.dequeue
      while(seen.contains(d._1)) {
        d = queue.dequeue
      }
      pos = d._1
      if(i % 10000 == 0){
        println(s"d: $d, seen.size: ${seen.size}, i: $i")
      }
      i+= 1

      assert(!seen.contains(pos))

      seen = seen + pos
      val nn = neighbors(pos)
          .filterNot(seen.contains)
          .filter(pos => inRange(pos,min,max))
          .map { p =>
            (p, manhattanDiffSum(p, sensorsAndBeacons, sensorManhattan))
          }
      queue ++= nn
    }

    println(pos)
    pos.x * 4000000 + pos.y
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

  def findStartX(
      y: Long,
      start0: Long,
      end0: Long,
      sensorsAndBeacons: Seq[(Vec2, Vec2)],
      sensorManhattan: Map[Vec2, Long]
  ): (Long, Long) = {
    var start = start0
    var end = end0
    while (start < end) {
      var mid = (start + end) / 2
      var startVal =
        manhattanDiffSum(Vec2(start, y), sensorsAndBeacons, sensorManhattan)
      var midVal =
        manhattanDiffSum(Vec2(mid, y), sensorsAndBeacons, sensorManhattan)
      var endVal =
        manhattanDiffSum(Vec2(end, y), sensorsAndBeacons, sensorManhattan)

      //println(s"start: $start, mid: $mid, end: $end, startVal: $startVal, midVal: $midVal, highVal: $endVal")

      if (startVal < midVal) {
        end = mid - 1
      } else if (endVal < midVal) {
        start = mid + 1
      } else {
        //println("same")
        val (left, leftVal) =
          findStartX(y, start, mid - 1, sensorsAndBeacons, sensorManhattan)
        val (right, rightVal) =
          findStartX(y, mid + 1, end, sensorsAndBeacons, sensorManhattan)
        //println(s"left: $left, leftVal: $leftVal, right: $right, rightVal: $rightVal")
        if (rightVal > leftVal) {
          //println("a")
          return (left, leftVal)
        } else if (leftVal > rightVal) {
          //println("b")
          return (right, rightVal)
        } else {
          return (right, rightVal)
        }
      }
    }
    (end, manhattanDiffSum(Vec2(end, y), sensorsAndBeacons, sensorManhattan))
  }

  def findStartY(
      x: Long,
      start0: Long,
      end0: Long,
      sensorsAndBeacons: Seq[(Vec2, Vec2)],
      sensorManhattan: Map[Vec2, Long]
  ): (Long, Long) = {
    var start = start0
    var end = end0
    while (start < end) {
      var mid = (start + end) / 2
      var startVal =
        manhattanDiffSum(Vec2(x, start), sensorsAndBeacons, sensorManhattan)
      var midVal =
        manhattanDiffSum(Vec2(x, mid), sensorsAndBeacons, sensorManhattan)
      var endVal =
        manhattanDiffSum(Vec2(x, end), sensorsAndBeacons, sensorManhattan)

      //println(s"start: $start, mid: $mid, end: $end, startVal: $startVal, midVal: $midVal, highVal: $endVal")

      if (startVal < midVal) {
        end = mid - 1
      } else if (endVal < midVal) {
        start = mid + 1
      } else {
        //println("same")
        val (left, leftVal) =
          findStartY(x, start, mid - 1, sensorsAndBeacons, sensorManhattan)
        val (right, rightVal) =
          findStartY(x, mid + 1, end, sensorsAndBeacons, sensorManhattan)
        //println(s"left: $left, leftVal: $leftVal, right: $right, rightVal: $rightVal")
        if (rightVal > leftVal) {
          //println("a")
          return (left, leftVal)
        } else if (leftVal > rightVal) {
          //println("b")
          return (right, rightVal)
        } else {
          //println("c")
          return (right, rightVal)
        }
      }
    }
    //println("end y")
    (end, manhattanDiffSum(Vec2(x, end), sensorsAndBeacons, sensorManhattan))
  }

  private def manhattanDiffSum(
      pos: Vec2,
      sensorsAndBeacons: Seq[(Vec2, Vec2)],
      sensorManhattan: Map[Vec2, Long]
  ) = {
    sensorsAndBeacons.map { case (sensor, beacon) =>
      val dist = manhattan(pos, sensor)
      dist - sensorManhattan(sensor)
    }.sum
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
