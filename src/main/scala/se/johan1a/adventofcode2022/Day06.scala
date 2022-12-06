package se.johan1a.adventofcode2022

object Day06 {

  def part1(input: String): Int = {
    var i = 4
    while (i < input.size && input.substring(i - 4, i).toSet.size != 4) {
      i += 1
    }
    i
  }

  def part2(input: String): Int = {
    var i = 14
    while (i < input.size && input.substring(i - 14, i).toSet.size != 14) {
      i += 1
    }
    i
  }
}
