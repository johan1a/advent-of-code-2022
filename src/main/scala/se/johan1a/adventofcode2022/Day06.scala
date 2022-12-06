package se.johan1a.adventofcode2022

object Day06 {

  def part1(input: Seq[String]): Int = {
    val line = input.head
    var i = 4
    println(line.substring(i-4,i).toSet)
    while (i < line.size && line.substring(i-4,i).toSet.size != 4) {
      i += 1
    }
    i
  }

  def part2(input: Seq[String]): Int = {
    val line = input.head
    var i = 14
    println(line.substring(i-14,i).toSet)
    while (i < line.size && line.substring(i-14,i).toSet.size != 14) {
      i += 1
    }
    i
  }
}
