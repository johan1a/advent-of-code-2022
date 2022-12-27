package se.johan1a.adventofcode2022

import scala.collection.mutable.Buffer

object Day25 {

  def part1(input: Seq[String]): String = {
    toSnafu(input.map(i => fromSnafu(i.trim())).sum)
  }

  def fromSnafu(input: String): Long = {
    var i = 0
    var sum = 0L
    while (i < input.size) {
      var k = 1L
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
    assert(toSnafu(sum) == input)
    sum
  }

  def toSnafu(input: Long): String = {
    if (input == 0) {
      ""
    } else {
      val m = input / 5
      val k = input % 5
      k match {
        case 0 => toSnafu(m) + "0"
        case 1 => toSnafu(m) + "1"
        case 2 => toSnafu(m) + "2"
        case 3 => toSnafu(m + 1) + "="
        case 4 => toSnafu(m + 1) + "-"
      }
    }
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
