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

  trait SideType
  case object Front extends SideType
  case object Top extends SideType
  case object Back extends SideType
  case object Bottom extends SideType
  case object Left extends SideType
  case object Right extends SideType

  // rotations are clockwise
  case class InputSide(
      side: SideType,
      offsetX: Int,
      offsetY: Int,
      rotationDegrees: Int
  )

  // input
  // .FR
  // .T
  // LB
  // U
  //
  // canonicatl
  // ..F
  // .LTR
  // ..B.
  // ..U
  def part2(
      input: Seq[String],
      order: Seq[InputSide],
      width: Int,
      startOffset: Int
  ): Int = {
    // needs input: which sides come in what order and in what orientation
    // also, how long is a side
    // orientation: rotate x * 90 degrees, and add offset, + 2 sides x, + 2 sides y
    // if pos > some y, turn and go to side x
    val (grid, path) = parse2(input, order, width)
    println(grid)

    var dir = right
    val startPos = Vec2(startOffset * width, 0)
    var pos = startPos
    println(s"start pos: $pos, dir: $dir")
    path.foreach { action =>
      // println(s"action: $action")
      action match {
        case Move(n) =>
          val (newPos, newDir) = move2(grid, width, startPos, pos, dir, n)
          pos = newPos
          dir = newDir
        case turnAction =>
          dir = turn(dir, turnAction)
      }
      // println(s"pos: $pos, dir: $dir")
    }

    //if we are in a flipped side, calculate back to original coord system
    //find current side type from coords
    //rotate back 360 - rotation degrees
    //calculate score
    -1
  }

  private def parse2(input: Seq[String], sides: Seq[InputSide], width: Int) = {
    val ss = split(input)
    val (lines, rawPath) = (ss.head, ss.last.head)

    var y = 0
    var x = 0
    var sidesToParse = sides
    val grid = mutable.Map[Vec2, Char]()
    while (sidesToParse.nonEmpty) {
      println((x, y))
      if (x == lines(y).size) {
        x = 0
        y += width
      } else if (lines(y).charAt(x) == ' ') {
        x += width
      } else {
        val side = sidesToParse.head
        sidesToParse = sidesToParse.tail
        var sideGrid = mutable.Map[Vec2, Char]()
        y.until(y + width).map { i =>
          x.until(x + width).map { j =>
            val pos = Vec2(j + side.offsetX * width, i + side.offsetY * width)
            sideGrid(pos) = lines(i).charAt(j)
          }
        }
        sideGrid = rotate(sideGrid, side.rotationDegrees, width)
        grid ++= sideGrid
        x += width
      }
    }
    (grid, parsePath(rawPath))
  }

  // original
  // (0,0), (1, 0), (2,0)
  // (0,1), (1, 1), (2,1)
  // (0,2), (1, 2), (2,2)
  // rotated once
  // (0,2), (0, 1), (0,0)
  // (1,2), (1, 1), (1,0)
  // (2,2), (2, 1), (2,0)
  // y1 := x0
  // x1 -> max - y0

  // rotate clockwise
  private def rotate(
      grid: mutable.Map[Vec2, Char],
      degrees: Int,
      width: Int
  ) = {
    var prevGrid = grid
    0.until(degrees / 90).foreach { _ =>
      val rotatedGrid = mutable.Map[Vec2, Char]()
      prevGrid.foreach { case (pos, char) =>
        rotatedGrid += (Vec2(width - pos.y, pos.x) -> char)
      }
      prevGrid = rotatedGrid
    }

    prevGrid
  }

  private def move2(
      grid: mutable.Map[Vec2, Char],
      width: Int,
      startPos: Vec2,
      pos: Vec2,
      dir: Int,
      n: Int
  ): (Vec2, Int) = {
    val (startX, startY) = (pos.x, pos.y)

    var changedPos = pos
    var changedDir = dir
    var nextPos = pos
    var nextDir = dir

    0.until(n).foreach { _ =>
      dir match {
        // right
        case 0 =>
          nextPos = add(changedPos, Vec2(1, 0))
          if (
            nextPos.y >= 0 && nextPos.y < width && nextPos.x > startPos.x + width
          ) {
            //front -> right
            nextPos = Vec2(startPos.x + 2 * width - 1 - nextPos.y, width)
            nextDir = down
          } else if (
            nextPos.y >= width && nextPos.y < 2 * width && nextPos.x > startPos.x + 2 * width
          ) {
            // right -> bottom
            nextPos = Vec2(startPos.x + width - 1, width - nextPos.y % width + 3 * width)
            nextDir = left
          } else if (
            nextPos.y >= 2 * width && nextPos.y < 3 * width && nextPos.x > startPos.x + width
          ) {
            // back -> right
            nextPos =
              Vec2(startPos.x + width + nextPos.y - 2 * width, 2 * width - 1)
            nextDir = up
          } else if (
            nextPos.y > 3 * width && nextPos.x > startPos.x + width
          ) {
            // bottom -> right?
            nextPos =
              Vec2(startPos.x + 2 * width - 1, 2 * width - nextPos.y % width)
            nextDir = left
          }
        // down
        case 1 =>
          nextPos = add(changedPos, Vec2(0, 1))

          if (
            nextPos.x >= startPos.x && nextPos.x < startPos.x + width && nextPos.y > 4 * width
          ) {
            // bottom -> front
            nextPos = Vec2(nextPos.x, 0)
            nextDir = down
          } else if (nextPos.x >= startPos.x + width && nextPos.y > 2 * width) {
            // right -> back
            nextPos = Vec2(startPos.x + width -1, startPos.y + width + nextPos.y % width)
            nextDir = left
          } else if (nextPos.x < startPos.x && nextPos.y > 2 * width) {
            // left -> back
            nextPos = Vec2(startPos.x, 2 * width + (nextPos.x - startPos.x).abs)
            nextDir = right
          } else {
            ???
          }
        // left
        case 2 =>
          nextPos = add(changedPos, Vec2(-1, 0))
          if (
            nextPos.y >= 0 && nextPos.y < width && nextPos.x < startPos.x
          ) {
            //front -> left
            nextPos = Vec2(startPos.x - 2 * width - 1 + nextPos.y, width)
            nextDir = down
          } else if (
            nextPos.y >= width && nextPos.y < 2 * width && nextPos.x < startPos.x - width
          ) {
            //left -> bottom
            nextPos = Vec2(startPos.x, width - nextPos.y % width + 3 * width)
            nextDir = right
          } else if (
            nextPos.y >= 2 * width && nextPos.y < 3 * width && nextPos.x < startPos.x
          ) {
            // back -> left
            nextPos = Vec2(startPos.x - (nextPos.y - 2 * width), 2 * width - 1)
            nextDir = up
          } else if (
            nextPos.y > 3 * width && nextPos.x < startPos.x
          ) {
            // bottom -> left
            nextPos = Vec2(startPos.x - width - 1, 2 * width - nextPos.y % width)
            nextDir = right
          }

        //up
        case 3 =>
          nextPos = add(changedPos, Vec2(0, -1))

          if (nextPos.x >= startPos.x && nextPos.x < startPos.x + width && nextPos.y < 0) {
            // front -> bottom
            nextPos = Vec2(nextPos.x, 4*width-1)
            nextDir = up
          } else if (nextPos.x >= startPos.x + width && nextPos.y > width) {
            // right -> front
            nextPos = Vec2(startPos.x + width - 1, width - nextPos.x % width)
            nextDir = left
          } else if (nextPos.x < startPos.x && nextPos.y > width) {
            // left -> front
            nextPos = Vec2(startPos.x, width - (nextPos.x - startPos.x).abs%width)
            nextDir = right
          } else {
            ???
          }

      }
      if (grid(nextPos) != '#') {
        changedPos = nextPos
        changedDir = nextDir
      } else {
        //println(s"hit a wall at ${nextPos}")
      }
    }

    (changedPos, changedDir)
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
        case 3 =>
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
