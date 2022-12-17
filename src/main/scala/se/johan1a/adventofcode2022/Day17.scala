package se.johan1a.adventofcode2022

import Utils._

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
      //println(s"startpos: $pos, highestY: $highestY i:$i")
      while (!atRest) {
        //println(s"pos: $pos")
        jets(j % jets.size) match {
          case '>' =>
            //println(s"applying jet j=$j >")
            pos = add(pos, Vec2(1, 0))
          case '<' =>
            //println(s"applying jet j=$j <")
            pos = add(pos, Vec2(-1, 0))
        }

        if (collision(static, pos, shape)) {
          pos = prevPos
        }
        prevPos = pos
        pos = add(pos, Vec2(0, 1))
        //println("moving down")
        if (collision(static, pos, shape)) {
          pos = prevPos
          atRest = true
        } else {
          prevPos = pos
        }

        // assert(j < 1000)
        j += 1
      }
      //println(s"end pos: $pos\n")
      static = static ++ (shape.points.map { point =>
        add(point, pos) -> true
      })

      //println(s"shape points: ${shape.points.map(p => add(p, pos))}")
      val shapeHighestY =
        shape.points.map(p => add(p, pos)).sortBy(_.y).head.y.toInt
      //println(s"shapeHighestY: $shapeHighestY")
      //println(s"wtf: ${shape.points.map(p => add(p, pos)).sortBy(_.y).head.y}")
      highestY = Math.min(highestY, shapeHighestY)

      // assert(i < 3)

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
      if (r0) {
        //println(s"wall collision at $pos, point:$point,base:$base")
      }
      if (r1) {
        //println(s"static collision at $pos")
      }
      r0 || r1
    }
  }

  def part2(input: Seq[String]): Int = {
    -1
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
