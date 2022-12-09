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

        println(s"pos: $pos, tailPos: $tailPos")
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

        val neighbors0 = neighbors(pos)
        if (!neighbors0.contains(tailPos) && tailPos != pos) {
          println(s"newPos: $pos")
          println(s"Neighbors: $neighbors0")
          println("Moving tail")
          val xPos: Long = pos.x
          val yPos: Long = pos.y
          tailPos = (tailPos.x, tailPos.y) match {
            case (x, y) if x == xPos && y == yPos - 2L =>
              println("11")
              Vec2(xPos, yPos - 1)

            case (x, y) if x == xPos && y == yPos + 2=>
              println("10")
              Vec2(xPos, yPos + 1)

            case (x, y) if x == xPos - 2 && y == yPos =>
              println("9")
              Vec2(xPos - 1, yPos)

            case (x, y) if x == xPos + 2 && y == yPos =>
              println("8")
              Vec2(xPos + 1, yPos)

            case (x, y) if x == xPos + 2 && y == yPos + 1 =>
              println("7")
              Vec2(xPos + 1, yPos)

            case (x, y) if x == xPos + 2 && y == yPos - 1=>
              println("6")
              Vec2(xPos + 1, yPos)

            case (x, y) if x == xPos - 2 && y == yPos + 1 =>
              println("5")
              Vec2(xPos - 1, yPos)

            case (x, y) if x == xPos - 2 && y == yPos - 1 =>
              println("4")
              Vec2(xPos - 1, yPos)

            case (x, y) if x == xPos + 1 && y == yPos + 2 =>
              println("3")
              Vec2(xPos, yPos + 1)

            case (x, y) if x == xPos + 1 && y == yPos - 2 =>
              println("2")
              Vec2(xPos, yPos - 1)

            case (x, y) if x == xPos - 1 && y == yPos + 2 =>
              println("1")
              Vec2(xPos, yPos + 1)

            case (x, y) if x == xPos - 1 && y == yPos - 2 =>
              println("0")
              Vec2(xPos, yPos - 1L)
          }
          println(s"new tailPos: $tailPos")

        }
        seen = seen + tailPos
      }
    }

    seen.size
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
