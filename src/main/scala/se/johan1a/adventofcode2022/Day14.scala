package se.johan1a.adventofcode2022

import Utils._

object Day14 {

  val sandSource = Vec2(500, 0)

  def part1(input: Seq[String]): Int = {
    var rocks: Set[Vec2] = input.flatMap(parseLine).toSet
    val bottom = rocks.toSeq.sortBy(_.y).reverse.head.y
    solve(rocks, bottom, p => p.y == bottom)
  }

  def part2(input: Seq[String]): Int = {
    var rocks: Set[Vec2] = input.flatMap(parseLine).toSet
    val bottom = rocks.toSeq.sortBy(_.y).reverse.head.y + 1
    solve(rocks, bottom, p => p == sandSource) + 1
  }

  def solve(
      startingRocks: Set[Vec2],
      bottomY: Long,
      endFunc: Vec2 => Boolean
  ): Int = {
    var rocks = startingRocks
    var sum = 0
    var shouldContinue = true
    var sandPos = sandSource
    var prevPos = sandPos
    while (shouldContinue) {
      prevPos = sandPos
      var nextSandPos = move(rocks, sandPos)
      while (nextSandPos != sandPos && nextSandPos.y < bottomY) {
        sandPos = nextSandPos
        nextSandPos = move(rocks, nextSandPos)
      }
      if (endFunc(nextSandPos)) {
        shouldContinue = false
      } else {
        sum += 1
        rocks = rocks + nextSandPos
        sandPos = prevPos
      }
    }

    sum
  }

  private def move(rocks: Set[Vec2], sandPos: Vec2): Vec2 = {
    val below = add(sandPos, Vec2(0, 1))
    val belowLeft = add(sandPos, Vec2(-1, 1))
    val belowRight = add(sandPos, Vec2(1, 1))
    if (!rocks.contains(below)) {
      below
    } else if (!rocks.contains(belowLeft)) {
      belowLeft
    } else if (!rocks.contains(belowRight)) {
      belowRight
    } else {
      sandPos
    }
  }

  private def parseLine(line: String): Seq[Vec2] = {
    val pairs = line.split(" -> ")
    val points: Seq[Vec2] = pairs.map((p: String) => {
      val numbers: Seq[String] = p.split(",")
      val x: String = numbers.head
      val y: String = numbers.last
      Vec2(x.toLong, y.toLong)
    })

    var linePoints = Seq[Vec2]()
    linePoints = linePoints ++ points
    var i = 0
    while (i < points.size - 1) {
      val a = points(i)
      var j = i + 1
      var b = points(j)
      val dir = sign(sub(b, a))
      var curr = a
      while (curr != b) {
        linePoints = linePoints :+ curr
        curr = add(curr, dir)
      }
      i += 1
    }
    linePoints
  }

}
