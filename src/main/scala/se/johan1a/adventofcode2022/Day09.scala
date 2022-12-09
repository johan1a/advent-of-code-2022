package se.johan1a.adventofcode2022

import Utils._

object Day09 {

  def part1(input: Seq[String]): Int = {
    solve(input, 2)
  }

  def part2(input: Seq[String]): Int = {
    solve(input, 10)
  }

  def solve(input: Seq[String], n: Int = 10): Int = {
    var knots = Array.fill(n)(Vec2(0, 0))
    var seen = Set[Vec2](knots.last)

    input.foreach { line =>
      line match {
        case s"$dir $n" =>
          0.until(n.toInt).map { _ =>
            knots(0) = move(knots.head, dir)
            knots.zipWithIndex.drop(1).foreach { case (knot, i) =>
              val pos = knots(i - 1)
              val tailPos = knots(i)
              val newTailPos = moveCloserTo(tailPos, pos)
              knots(i) = newTailPos
            }
            seen = seen + knots.last
          }
      }
    }
    seen.size
  }

  private def moveCloserTo(tailPos: Vec2, pos: Vec2): Vec2 = {
    if (!neighbors(pos).contains(tailPos)) {
      val diff = sub(pos, tailPos)
      add(tailPos, sign(diff))
    } else {
      tailPos
    }
  }
}
