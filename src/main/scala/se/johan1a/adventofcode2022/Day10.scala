package se.johan1a.adventofcode2022

import scala.collection.mutable.Queue

object Day10 {

  case class Effect(var ticksLeft: Int, amountToAdd: Int)

  def part1(input: Seq[String]): Int = {
    solve(input)._1
  }

  def part2(input: Seq[String]): Seq[String] = {
    solve(input)._2
  }

  private def solve(input: Seq[String]): (Int, Seq[String]) = {
    val screenWidth = 40
    var x = 1
    var i = 1
    var queue = Queue[Effect]()
    var pixels = Seq[String]()
    var totalSignalStrength = 0

    while (i < input.size + 1 || queue.nonEmpty) {
      if (i < input.size + 1) {
        val line = input(i - 1)
        line match {
          case "noop" =>
            queue += Effect(1, 0)
          case s"addx $n" =>
            val k = n.toInt
            queue += Effect(2, k)
        }
      }

      if ((i - 20) % 40 == 0) {
        totalSignalStrength += i * x
      }

      val pixelPos = (i - 1) % screenWidth
      val diff = (x - pixelPos).abs
      if (diff <= 1) {
        pixels = pixels :+ "#"
      } else {
        pixels = pixels :+ "."
      }

      if (queue.nonEmpty) {
        queue.head.ticksLeft -= 1
      }
      if (queue.nonEmpty && queue.head.ticksLeft == 0) {
        val effect = queue.dequeue()
        x += effect.amountToAdd
      }

      i += 1
    }

    (totalSignalStrength, pixels.grouped(screenWidth).map(_.mkString).toSeq)
  }
}
