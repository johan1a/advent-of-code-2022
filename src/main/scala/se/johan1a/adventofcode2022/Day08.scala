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

    val borderSize = 2 * xSize + 2 * ySize - 4
    seen.size + borderSize
  }

  def part2(input: Seq[String]): Int = {
    val xSize = input.head.size
    val ySize = input.size
    var best = -1

    0.until(ySize).foreach { y =>
      0.until(xSize).foreach { x =>
        var height = input(y).charAt(x)
        var product = 1

        var y1 = y
        var x1 = x + 1
        var size = 0
        while (x1 < xSize) {
          size += 1
          if (input(y1).charAt(x1) >= height) {
            x1 = xSize
          }
          x1 += 1
        }
        println(size)
        product = product * size

        y1 = y
        x1 = x - 1
        size = 0
        while (x1 >= 0) {
          size += 1
          if (input(y1).charAt(x1) >= height) {
            x1 = -1
          }
          x1 -= 1
        }
        println(size)
        product = product * size

        y1 = y + 1
        x1 = x
        size = 0
        while (y1 < ySize) {
          size += 1
          if (input(y1).charAt(x1) >= height) {
            y1 = ySize
          }
          y1 += 1
        }
        println(size)
        product = product * size

        y1 = y - 1
        x1 = x
        size = 0
        while (y1 >= 0) {
          size += 1
          if (input(y1).charAt(x1) >= height) {
            y1 = -1
          }
          y1 -= 1
        }
        println(size)
        product = product * size

        println(s"x: $x, y: $y, product: $product")
        if (product > best) {
          best = product
        }
      }
    }

    best
  }
}
