package se.johan1a.adventofcode2022

object Day25 {

  def part1(input: Seq[String]): String = {
    toSnafu(input.map(i => fromSnafu(i.trim())).sum)
  }

  def fromSnafu(input: String): Long = {
    var i = 0
    var sum = 0L
    var p = 0
    while (i < input.size) {
      var k = 1
      0.until(i).foreach { _ =>
        k = k * 5
      }
      val c = input.charAt(input.size - 1 - i)
      val d = c.toString match {
        case "=" => -2 * k
        case "-" => -1 * k
        case "0" => 0
        case "1" => k
        case "2" => 2 * k
      }
      sum = sum + d
      i += 1
    }
    //println(s"input:$input, sum:$sum")
    sum
  }

  def toSnafu(input: Long): String = {
    println(s"Converting $input...")

    var res = ""

    var r = input + 12
    var k = 0
    while (Math.pow(5, k) < r) {
      k += 1
    }
    println(s"k: $k")

    while (k > 0) {
      k = k - 1
      var l = 4
      while (l * Math.pow(5, k) > r) {
        l -= 1
      }
      res += digit(l)
      r = r - l * 5 * k
      println(s"k: $k, l: $l, res: $res, r: $r")
    }

    res
  }

  private def digit(n: Long) = {
    n match {
      case 0 => "="
      case 1 => "-"
      case 2 => "0"
      case 3 => "1"
      case 4 => "2"
    }
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
