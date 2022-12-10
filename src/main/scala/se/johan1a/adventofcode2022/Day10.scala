package se.johan1a.adventofcode2022

object Day10 {

  case class Effect(var i: Int, n: Int)

  def part1(input: Seq[String]): Int = {
    var x = 1
    var i = 1
    var queue = Seq[Effect]()
    var sum = 0

    while (i < input.size + 1) {
      val line = input(i - 1)
      line match {
        case "noop" =>
          queue = queue :+ Effect(1, 0)
        case s"addx $n" =>
          val k = n.toInt
          queue = queue :+ Effect(2, k)
      }

      if ((i - 20) % 40 == 0) {
        sum += i * x
      }

      if (queue.nonEmpty) {
        queue.head.i -= 1
      }
      if (queue.nonEmpty && queue.head.i == 0) {
        val effect = queue.head
        queue = queue.drop(1)
        x += effect.n
      }

      i += 1
    }
    while (queue.nonEmpty) {
      if ((i - 20) % 40 == 0) {
        sum += i * x
      }
      if (queue.nonEmpty) {
        queue.head.i -= 1
      }
      if (queue.nonEmpty && queue.head.i == 0) {
        val effect = queue.head
        queue = queue.drop(1)
        x += effect.n
      }

      i += 1
    }
    sum
  }

  def part2(input: Seq[String]): Seq[String] = {
    val width = 40
    val height = 6
    var x = 1
    var i = 1
    var queue = Seq[Effect]()
    var pixels = Seq[String]()

    while (i < input.size + 1) {
      val line = input(i - 1)
      // println(line)
      line match {
        case "noop" =>
          queue = queue :+ Effect(1, 0)
        case s"addx $n" =>
          val k = n.toInt
          queue = queue :+ Effect(2, k)
      }

      if ((i - 20) % 40 == 0) {
        println(s"i: $i, x: $x")
      }

      val pixelPos = (i-1) % width
      val diff = (x - pixelPos).abs
      println(s"pixelPos: $pixelPos, x: $x, diff: $diff")
      if (diff <= 1) {
        pixels = pixels :+ "#"
      } else {
        pixels = pixels :+ "."
      }

      if (queue.nonEmpty) {
        queue.head.i -= 1
      }
      if (queue.nonEmpty && queue.head.i == 0) {
        val effect = queue.head
        queue = queue.drop(1)
        x += effect.n
      }

      i += 1
    }
    println("after instructions")
    while (queue.nonEmpty) {
      if ((i - 20) % 40 == 0) {
        println(s"i: $i, x: $x")
      }

      val pixelPos = (i-1) % width
      if ((x - pixelPos).abs <= 1) {
        pixels = pixels :+ "#"
      } else {
        pixels = pixels :+ "."
      }

      if (queue.nonEmpty) {
        queue.head.i -= 1
      }
      if (queue.nonEmpty && queue.head.i == 0) {
        val effect = queue.head
        queue = queue.drop(1)
        x += effect.n
      }

      i += 1
    }
    println(s"i: $i, x: $x, queue: $queue")
    pixels.grouped(width).map(_.mkString).toSeq
  }
}
