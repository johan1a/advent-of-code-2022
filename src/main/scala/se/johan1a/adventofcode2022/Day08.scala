package se.johan1a.adventofcode2022

import Utils._

object Day08 {

  private val lowestChar = ('0'.toInt - 1).toChar

  def part1(input: Seq[String]): Int = {
    val grid = makeGrid(input)

    straightPathsFromOutside(grid)
      .flatMap { paths =>
        paths.flatMap { path =>
          var highest = lowestChar
          path.filter { case Vec2(x, y) =>
            val height = grid(y.toInt)(x.toInt)
            val isHigher = height > highest
            if (isHigher) {
              highest = height
            }
            isHigher
          }
        }
      }
      .toSet
      .size
  }

  def part2(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    allPositions(grid).map { pos =>
      straightPathsFromPos(grid, pos).map { (path: Seq[Vec2]) =>
        val treeHeight = grid(pos.y.toInt)(pos.x.toInt)
        var prevHeight = lowestChar
        path.takeWhile { case Vec2(x, y) =>
          val height = grid(y.toInt)(x.toInt)
          val canSee = prevHeight < treeHeight
          prevHeight = height
          canSee
        }.size
      }.product
    }.max
  }
}
