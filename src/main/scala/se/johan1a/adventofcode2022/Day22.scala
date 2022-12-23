package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable
import scala.io.StdIn.readLine

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
    path.foreach { action =>
      action match {
        case Move(n) =>
          pos = move(grid, xBounds, yBounds, pos, dir, n)
        case turnAction =>
          dir = turn(dir, turnAction)
      }

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

  def part2(
      input: Seq[String],
      startPos: Vec2 = Vec2(50, 0),
      testPath: String = ""
  ): (Vec2, Int) = {
    val width = 50
    val (grid, parsedPath) = parse2(input, width)

    val path = if(testPath.nonEmpty) { parsePath(testPath) } else { parsedPath }

    var dir = right
    var pos = startPos

    var manual = false

    path.foreach { action =>
      println(s"\npos: $pos, dir: ${dirString(dir)}")
      println(s"next action: ${action}")

      if(manual) {
        val input = readLine()
        if (input == "exit") {
          throw new Exception
        }
      }

      action match {
        case Move(n) =>
          val (newPos, newDir) = move2(grid, width, startPos, pos, dir, n)
          pos = newPos
          dir = newDir
        case turnAction =>
          dir = turn(dir, turnAction)
      }
    }

    println(s"last pos: ${pos}, dir: ${dirString(dir)}")
    (pos, dir)
  }

  //.FR
  //.T
  //LB
  //U
  //
  //start: (50,0)
  //Right
  //
  //
  //Down
  //
  //Left
  //
  //Up
  //
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

    var hitWallPos: Option[Vec2] = None

    0.until(n).foreach { _ =>
      nextDir match {
        //.FR
        //.T
        //LB
        //U
        // right
        case 0 =>
          nextPos = add(changedPos, Vec2(1, 0))
          //println(s"nextPos before: $nextPos")
          if (nextPos.y < width && nextPos.x >= 3 * width) {
            println("R -> B")
            nextPos = add(changedPos, Vec2(-width, 2 * width))
            nextDir = left
          } else if (
            nextPos.y >= width && nextPos.y < 2 * width && nextPos.x >= 2 * width
          ) {
            nextPos = Vec2(2 * width + changedPos.y % width, width - 1)
            println("T -> R, nextPos: $nextPos")
            nextDir = up
          } else if (
            nextPos.y >= 2 * width && nextPos.y < 3 * width && nextPos.x >= 2 * width
          ) {
            println("B -> R")
            nextPos = Vec2(3 * width - 1, width - changedPos.y % width)
            nextDir = left
          } else if (
            nextPos.y >= 3 * width && nextPos.y < 4 * width && nextPos.x >= width
          ) {
            println("U -> B")
            nextPos = Vec2(width + nextPos.y % width, 3 * width - 1)
            nextDir = up
          }

        // down
        //.FR
        //.T
        //LB
        //U
        case 1 =>
          nextPos = add(changedPos, Vec2(0, 1))
          //println(s"nextPos: $nextPos")
          if (nextPos.x < width && nextPos.y >= 4 * width) {
            nextPos = Vec2(changedPos.x + 2 * width, 0)
            println(s"U -> R, nextPos: $nextPos")
            nextDir = down
          } else if (
            nextPos.x >= width && nextPos.x < 2 * width && nextPos.y >= 3 * width
          ) {
            println("B -> U")
            nextPos = Vec2(startPos.x - 1, 3 * width + changedPos.x % width)
            nextDir = left
          } else if (nextPos.x >= 2 * width && nextPos.y >= width) {
            println("R -> T")
            nextPos = Vec2(
              2 * width - 1,
              width + changedPos.x % width
            )
            nextDir = left
          }

        //left
        //.FR
        //.T
        //LB
        //U
        case 2 =>
          nextPos = add(changedPos, Vec2(-1, 0))
          if (nextPos.y < width && nextPos.x < width) {
            nextPos = Vec2(0, 2 * width + width - (changedPos.y % width))
            println(s"F -> L, nextPos: $nextPos")
            nextDir = right
          } else if (
            nextPos.y >= width && nextPos.y < 2 * width && nextPos.x < width
          ) {
            println("T -> L")
            nextPos = Vec2(changedPos.y % width, 2 * width)
            nextDir = down
          } else if (
            nextPos.y >= 2 * width && nextPos.y < 3 * width && nextPos.x < 0
          ) {
            println("L -> F")
            nextPos = Vec2(width, width - changedPos.y % width)
            nextDir = right
          } else if (
            nextPos.y >= 3 * width && nextPos.y < 4 * width && nextPos.x < 0
          ) {
            println("U -> F")
            nextPos = Vec2(nextPos.y % width + width, 0)
            nextDir = down
          }
        // up
        //.FR
        //.T
        //LB
        //U
        case 3 =>
          nextPos = add(changedPos, Vec2(0, -1))
          if (
            nextPos.x < width && nextPos.y < 2 * width
          ) {
            nextPos = Vec2(width, width + changedPos.x % width)
            println(s"L -> T, nextPos: $nextPos")
            nextDir = right
          } else if (
            nextPos.x >= width && nextPos.x < 2 * width && nextPos.y < 0
          ) {
            println("F -> U")
            nextPos = Vec2(0, 3 * width + changedPos.x % width)
            nextDir = right
          } else if (
            nextPos.x >= 2 * width && nextPos.x < 3 * width && nextPos.y < 0
          ) {
            println(s"nextPos before: $nextPos")
            nextPos = Vec2(changedPos.x % width, 4 * width - 1)
            println(s"R -> U, nextPos: $nextPos")
            nextDir = up
          }
      }
      if (grid(nextPos) != '#') {
        changedPos = nextPos
        changedDir = nextDir
      } else {
        //println(s"hit a wall at ${nextPos}")
        hitWallPos = Some(nextPos)
      }
    }

    if (hitWallPos.isDefined) {
      println(s"hit a wall at ${hitWallPos.get}")
    }

    (changedPos, changedDir)
  }

  private def printGrid(grid: mutable.Map[Vec2, Char], width: Int) = {
    0.until(width * 5).map { y =>
      0.until(width * 4).map { x =>
        print(grid.getOrElse(Vec2(x, y), " "))
      }
      println()
    }
  }

  private def parse2(input: Seq[String], width: Int) = {
    val ss = split(input)
    val (lines, rawPath) = (ss.head, ss.last.head)

    var y = 0
    var x = 0
    val grid = mutable.Map[Vec2, Char]()
    while (y < lines.size) {
      if (x == lines(y).size) {
        x = 0
        y = y + width
      } else if (lines(y).charAt(x) == ' ') {
        x = x + width
      } else {
        y.until(y + width).map { i =>
          x.until(x + width).map { j =>
            val pos = Vec2(j, i)
            grid(pos) = lines(i).charAt(j)
          }
        }
        x = x + width
      }
    }
    (grid, parsePath(rawPath))
  }

  def gridString(grid: mutable.Map[Vec2, Char]): String = {
    val minX: Long = grid.keys.minBy(_.x).x
    val minY: Long = grid.keys.minBy(_.y).y
    val maxX: Long = grid.keys.maxBy(_.x).x
    val maxY: Long = grid.keys.maxBy(_.y).y
    println((minX, maxX, minY, maxY))
    var str = ""
    minY.to(maxY).map { y =>
      minX.to(maxX).map { x =>
        str += grid.getOrElse(Vec2(x, y), " ")
      }
      str += "\n"
    }
    str
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

  private def dirString(dir: Int) = {
    dir match {
      case 0 => "right"
      case 1 => "down"
      case 2 => "left"
      case 3 => "up"
    }
  }
}
