package se.johan1a.adventofcode2022

object Day06 {

  def part1(input: String): Int = {
    solve(input, 4)
  }

  def part2(input: String): Int = {
    solve(input, 14)
  }

  private def solve(input: String, n: Int): Int = {
    input.sliding(n).indexWhere(window => window.toSet.size == n) + n
  }
}
