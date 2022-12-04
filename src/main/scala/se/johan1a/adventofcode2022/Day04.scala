package se.johan1a.adventofcode2022

import Utils._

object Day04 {

  def part1(input: Seq[String]): Int = {
    val res = input
      .filter(line => {
        val split = line.split(",")
        val a = split.head.split("-").map(_.toLong)
        val b = split.last.split("-").map(_.toLong)
        (a.head >= b.head && a.last <= b.last) || (b.head >= a.head && b.last <= a.last)
      })
      .map(x => {
        x
      })

    res.size
  }

  def part2(input: Seq[String]): Int = {
    input
      .filter(line => {
        val split = line.split(",")
        val a = split.head.split("-").map(_.toLong)
        val b = split.last.split("-").map(_.toLong)
        val overlap =
          !(a.head > b.last || a.last < b.head || b.head > a.last || b.last < a.head)

        overlap
      })
      .size
  }
}
