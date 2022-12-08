package se.johan1a.adventofcode2022

object Day08 {

  def part1(input: Seq[String]): Int = {
    val xSize = input.head.size
    val ySize = input.size

    var seen = Set[(Int, Int)]()

    1.until(ySize - 1).map { y =>
      var highest = input(y).charAt(0)
      1.until(xSize - 1).map { x =>
        val pos = (x, y)
        val height = input(y).charAt(x)
        if (height > highest) {
          highest = height
          if (!seen.contains(pos)) {
            seen = seen + pos
          }
        }
      }
    }

    (1).until(ySize - 1).foreach { y =>
      var highest = input(y).charAt(xSize - 1)
      (1).until(xSize - 1).reverse.foreach { x =>
        val pos = (x, y)
        val height = input(y).charAt(x)
        if (height > highest) {
          highest = height
          if (!seen.contains(pos)) {
            seen = seen + pos
          }
        }
      }
    }

    (1).until(xSize - 1).foreach { x =>
      var highest = input(0).charAt(x)
      (1).until(ySize - 1).foreach { y =>
        val pos = (x, y)
        val height = input(y).charAt(x)
        if (height > highest) {
          highest = height
          if (!seen.contains(pos)) {
            seen = seen + pos
          }
        }
      }
    }

    (1).until(xSize - 1).foreach { x =>
      var highest = input(ySize - 1).charAt(x)
      (1).until(ySize - 1).reverse.foreach { y =>
        val pos = (x, y)
        val height = input(y).charAt(x)
        if (height > highest) {
          highest = height
          if (!seen.contains(pos)) {
            seen = seen + pos
          }
        }
      }
    }

    seen.foreach(println)
    val borderSize = 2 * xSize + 2 * ySize - 4
    println(borderSize)
    seen.size + borderSize
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
