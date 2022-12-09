package se.johan1a.adventofcode2022

import Utils._

object Day09 {

  def part1(input: Seq[String]): Int = {
    var pos = Vec2(0, 0)
    var tailPos = Vec2(0, 0)
    var seen = Set[Vec2](tailPos)

    input.foreach { line =>
      val split = line.split(" ")
      val dir = split.head
      val n = split.last.toInt
      0.until(n).map { _ =>

        dir match {
          case "U" =>
            pos = Vec2(pos.x, pos.y - 1)
          case "D" =>
            pos = Vec2(pos.x, pos.y + 1)
          case "L" =>
            pos = Vec2(pos.x - 1, pos.y)
          case "R" =>
            pos = Vec2(pos.x + 1, pos.y)
        }

        tailPos = moveCloserTo(tailPos, pos)
        seen = seen + tailPos
      }
    }

    seen.size
  }

  def part2(input: Seq[String]): Int = {
    var knots = Array.fill(10)(Vec2(0, 0))
    var seen = Set[Vec2](knots.last)

    input.foreach { line =>
      val split = line.split(" ")
      val dir = split.head
      val n = split.last.toInt
      0.until(n).map { _ =>

        val pos = knots.head
        val newPos = dir match {
          case "U" =>
            Vec2(pos.x, pos.y - 1)
          case "D" =>
            Vec2(pos.x, pos.y + 1)
          case "L" =>
            Vec2(pos.x - 1, pos.y)
          case "R" =>
            Vec2(pos.x + 1, pos.y)
        }
        knots(0) = newPos
        var i = 1
        while(i < knots.size) {
          val pos = knots(i-1)
          val tailPos = knots(i)
          val newTailPos = moveCloserTo(tailPos, pos)
          knots(i) = newTailPos
          println(s"i:$i, pos: $pos, tailPos: $tailPos, newTailPos: $newTailPos")
          i += 1
        }
        seen = seen + knots.last
        println()
        knots.foreach(println)
      }
    }

    seen.size
  }

  private def moveCloserTo(tailPos: Vec2, pos: Vec2): Vec2 = {
    val neighbors0 = neighbors(pos)
    if (!neighbors0.contains(tailPos) && tailPos != pos) {
      val xPos: Long = pos.x
      val yPos: Long = pos.y
      (tailPos.x, tailPos.y) match {
        case (x, y) if x == xPos && y == yPos - 2L =>
          Vec2(xPos, yPos - 1)

        case (x, y) if x == xPos && y == yPos + 2=>
          Vec2(xPos, yPos + 1)

        case (x, y) if x == xPos - 2 && y == yPos =>
          Vec2(xPos - 1, yPos)

        case (x, y) if x == xPos + 2 && y == yPos =>
          Vec2(xPos + 1, yPos)

        case (x, y) if x == xPos + 2 && y == yPos + 1 =>
          Vec2(xPos + 1, yPos)

        case (x, y) if x == xPos + 2 && y == yPos - 1=>
          Vec2(xPos + 1, yPos)

        case (x, y) if x == xPos - 2 && y == yPos + 1 =>
          Vec2(xPos - 1, yPos)

        case (x, y) if x == xPos - 2 && y == yPos - 1 =>
          Vec2(xPos - 1, yPos)

        case (x, y) if x == xPos + 1 && y == yPos + 2 =>
          Vec2(xPos, yPos + 1)

        case (x, y) if x == xPos + 1 && y == yPos - 2 =>
          Vec2(xPos, yPos - 1)

        case (x, y) if x == xPos - 1 && y == yPos + 2 =>
          Vec2(xPos, yPos + 1)

        case (x, y) if x == xPos - 1 && y == yPos - 2 =>
          Vec2(xPos, yPos - 1L)

        case (x, y) if x == xPos - 2 && y == yPos - 2 =>
          Vec2(xPos - 1, yPos - 1L)

        case (x, y) if x == xPos + 2 && y == yPos + 2 =>
          Vec2(xPos + 1, yPos + 1L)

        case (x, y) if x == xPos + 2 && y == yPos - 2 =>
          Vec2(xPos + 1, yPos - 1L)

        case (x, y) if x == xPos - 2 && y == yPos + 2 =>
          Vec2(xPos - 1, yPos + 1L)
      }
    } else {
      tailPos
    }

  }
}
