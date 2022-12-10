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
      // println(line)
      line match {
        case "noop" =>
          queue = queue :+ Effect(1, 0)
        case s"addx $n" =>
          val k = n.toInt
          queue = queue :+ Effect(2, k)
      }

      if ((i - 20) % 40 == 0) {
        sum += i * x
        println(s"i: $i, x: $x, strength: ${i * x}, total: $sum")
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
        sum += i * x
        println(s"i: $i, x: $x, strength: ${i * x}, total: $sum")
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
    sum
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
