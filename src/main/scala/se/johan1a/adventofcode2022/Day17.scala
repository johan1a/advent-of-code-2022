package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.Map

object Day17 {

  case class Shape(points: Seq[Vec2])

  val minX = 0
  val maxX = 8
  val maxY = 0
  val minPos = Vec2(minX + 1, Int.MinValue)
  val maxPos = Vec2(maxX, maxY)

  def part1(input: Seq[String], n: Int = 2022): Int = {
    val jets = input.head.toCharArray()
    var highestY = 0
    var i = 0
    var static = Map[Vec2, Boolean]().withDefaultValue(false)
    var j = 0
    while (i < n) {
      val shape = shapes(i % shapes.size)
      var pos = Vec2(minX + 3, highestY - 4)
      var prevPos = pos
      var atRest = false
      while (!atRest) {
        jets(j % jets.size) match {
          case '>' =>
            pos = add(pos, Vec2(1, 0))
          case '<' =>
            pos = add(pos, Vec2(-1, 0))
        }

        if (collision(static, pos, shape)) {
          pos = prevPos
        }
        prevPos = pos
        pos = add(pos, Vec2(0, 1))
        if (collision(static, pos, shape)) {
          pos = prevPos
          atRest = true
        } else {
          prevPos = pos
        }

        j += 1
      }
      static = static ++ (shape.points.map { point =>
        add(point, pos) -> true
      })

      val shapeHighestY =
        shape.points.map(p => add(p, pos)).sortBy(_.y).head.y.toInt
      highestY = Math.min(highestY, shapeHighestY)

      i += 1
    }

    highestY.abs
  }

  def collision(
      static: Map[Vec2, Boolean],
      base: Vec2,
      shape: Shape
  ): Boolean = {
    shape.points.exists { point =>
      val pos = add(point, base)
      val r0 = !inRange(pos, minPos, maxPos)
      val r1 = static(pos)
      r0 || r1
    }
  }

  def part2(input: Seq[String], n: Long = 1000000000000L): Long = {
    val jets = input.head.toCharArray()
    var highestY = 0L
    var i = 0L
    var static = Map[Vec2, Boolean]().withDefaultValue(false)
    var j = 0

    var seen = Map[(Int, Int, String), (Long, Long)]()
    var useSeen = true

    while (i < n) {
      val shape = shapes((i % shapes.size).toInt)
      var pos = Vec2(minX + 3, highestY - 4)
      var prevPos = pos
      var atRest = false
      // println(s"i:$i, highestY: $highestY, pos: $pos")

      while (!atRest) {
        jets(j % jets.size) match {
          case '>' =>
            pos = add(pos, Vec2(1, 0))
          case '<' =>
            pos = add(pos, Vec2(-1, 0))
        }

        if (collision(static, pos, shape)) {
          // println("wall collision")
          pos = prevPos
        }
        prevPos = pos
        pos = add(pos, Vec2(0, 1))
        if (collision(static, pos, shape)) {
          // println("static collision")
          pos = prevPos
          atRest = true
        } else {
          prevPos = pos
        }

        j += 1
      }
      static = static ++ (shape.points.map { point =>
        add(point, pos) -> true
      })

      val shapeHighestY =
        shape.points.map(p => add(p, pos)).sortBy(_.y).head.y.toInt
      highestY = Math.min(highestY, shapeHighestY)

      val stateDepth = 50
      val topState = shapeHighestY
        .until(shapeHighestY + stateDepth)
        .map { y =>
          (minX + 1)
            .until(maxX)
            .map { x =>
              if (static(Vec2(x, y))) then "#" else "."
            }
            .mkString
        }
        .mkString

      if (i % 113 == 0) {
        println(highestY / 178)
      }

      val state = ((i % shapes.size).toInt, j % jets.size, topState)
      if (useSeen && seen.contains(state)) {
        val (oldI, oldHighestY) = seen(state)
        // println(s"seen state at i:$i, highestY: ${highestY.abs}")
        val period = i - oldI
        val yPeriod = highestY - oldHighestY
        println(
          s"period: $period, yPeriod: $yPeriod, oldhighesty: $oldHighestY, highestY: $highestY"
        )
        val k = (n - i) / period
        i = i + k * period
        highestY = highestY + k * yPeriod

        0.until(stateDepth)
          .map { yDiff =>
            val oldY = oldHighestY + yDiff
            val newY = highestY + yDiff
            (minX + 1)
              .until(maxX)
              .map { x =>
                if (static(Vec2(x, oldY))) {
                  static(Vec2(x, newY)) = true
                }
              }
              .mkString
          }
          .mkString

        println(s"i: $i, highestY: $highestY")
        useSeen = false
      }
      seen = seen + (state -> (i, highestY))

      i += 1
    }

    highestY.abs
  }

  val shapes = Seq(
    Shape(
      Seq(
        Vec2(0, 0),
        Vec2(1, 0),
        Vec2(2, 0),
        Vec2(3, 0)
      )
    ),
    Shape(
      Seq(
        Vec2(1, -2),
        Vec2(0, -1),
        Vec2(1, -1),
        Vec2(2, -1),
        Vec2(1, 0)
      )
    ),
    Shape(
      Seq(
        Vec2(2, -2),
        Vec2(2, -1),
        Vec2(0, 0),
        Vec2(1, 0),
        Vec2(2, 0)
      )
    ),
    Shape(
      Seq(
        Vec2(0, -3),
        Vec2(0, -2),
        Vec2(0, -1),
        Vec2(0, 0)
      )
    ),
    Shape(
      Seq(
        Vec2(0, -1),
        Vec2(1, -1),
        Vec2(0, 0),
        Vec2(1, 0)
      )
    )
  )
}
