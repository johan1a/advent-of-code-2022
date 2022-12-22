package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable

object Day22 {

  trait Action
  case class Move(n: Int) extends Action
  case object TurnLeft extends Action
  case object TurnRight extends Action

  val right = 0
  val down = 1
  val left = 2
  val up = 3

  def part1(input: Seq[String]): Int = {
    val (xBounds, yBounds, grid, path) = parse(input)
    var dir = right
    var pos = Vec2(xBounds(0)._1, 0)
    println(s"start pos: $pos, dir: $dir")
    path.foreach { action =>
      // println(s"action: $action")
      action match {
        case Move(n) =>
          pos = move(grid, xBounds, yBounds, pos, dir, n)
        case turnAction =>
          dir = turn(dir, turnAction)
      }
      // println(s"pos: $pos, dir: $dir")
    }

    1000 * (pos.y.toInt + 1) + 4 * (pos.x.toInt + 1) + dir
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  private def move(
      grid: mutable.Map[Vec2, Char],
      xBounds: mutable.Map[Long, (Long, Long)],
      yBounds: mutable.Map[Long, (Long, Long)],
      pos: Vec2,
      dir: Int,
      n: Int
  ) = {
    val (startX, startY) = (pos.x, pos.y)

    var changedPos = pos
    var nextPos = pos

    0.until(n).foreach { _ =>
      dir match {
        case 0 =>
          nextPos = add(changedPos, Vec2(1, 0))
          if (nextPos.x > xBounds(startY)._2) {
            //println(s"nextpos: $nextPos, xBounds(startY)._2: ${xBounds(startY)._2}")
            nextPos = Vec2(xBounds(startY)._1, nextPos.y)
          }
        case 1 =>
          nextPos = add(changedPos, Vec2(0, 1))
          if (nextPos.y > yBounds(startX)._2) {
            nextPos = Vec2(startX, yBounds(startX)._1)
          }
        case 2 =>
          nextPos = add(changedPos, Vec2(-1, 0))
          if (nextPos.x < xBounds(startY)._1) {
            nextPos = Vec2(xBounds(startY)._2, nextPos.y)
          }
        case 3   =>
          nextPos = add(changedPos, Vec2(0, -1))
          if (nextPos.y < yBounds(startX)._1) {
            nextPos = Vec2(startX, yBounds(startX)._2)
          }
      }
      if (grid(nextPos) != '#') {
        changedPos = nextPos
      } else {
        //println(s"hit a wall at ${nextPos}")
      }
    }

    changedPos
  }

  private def turn(orientation: Int, direction: Action): Int = {
    assert(orientation >= 0 && orientation < 4)

    (orientation, direction) match {
      case (orientation, TurnLeft) =>
        if (orientation == 0) {
          3
        } else {
          orientation - 1
        }
      case (orientation, TurnRight) => (orientation + 1) % 4
      case _                        => ???
    }
  }

  private def parse(input: Seq[String]) = {
    val ss = split(input)
    val (lines, rawPath) = (ss.head, ss.last.head)

    val grid = mutable.Map[Vec2, Char]()
    val xBounds = mutable.Map[Long, (Long, Long)]()
    val yBounds = mutable.Map[Long, (Long, Long)]()

    lines.indices.map { y =>
      var minX = lines(y).size - 1
      var maxX = 0
      lines(y).indices.map { x =>
        lines(y)(x) match {
          case ' ' =>
          case char =>
            grid += (Vec2(x, y) -> char)
            if (x < minX) {
              minX = x
            }
            if (x > maxX) {
              maxX = x
            }

            val (currentMinY, currentMaxY) =
              yBounds.getOrElse(x, ((lines.size - 1).toLong, 0L))
            val minY = Math.min(y.toLong, currentMinY)
            val maxY = Math.max(y.toLong, currentMaxY)
            yBounds += (x.toLong -> (minY.toLong, maxY.toLong))
        }
      }
      xBounds += (y.toLong -> (minX.toLong, maxX.toLong))
    }
    (xBounds, yBounds, grid, parsePath(rawPath))
  }

  private def parsePath(raw: String) = {
    var i = 0
    var actions = Seq[Action]()
    while (i < raw.size) {
      if (raw.charAt(i).isDigit) {
        var j = i + 1
        while (j < raw.size && raw.charAt(j).isDigit) {
          j += 1
        }
        actions = actions :+ Move(raw.substring(i, j).toInt)
        i = j
      } else {
        raw.charAt(i) match {
          case 'L' => actions = actions :+ TurnLeft
          case 'R' => actions = actions :+ TurnRight
        }
        i += 1
      }
    }
    actions
  }
}
