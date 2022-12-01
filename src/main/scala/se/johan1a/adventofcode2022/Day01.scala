package se.johan1a.adventofcode2022

object Day01 {

  def part1(input: Seq[String]): Int = {
    var i = 0
    var elves = Seq[Int]()
    while (i < input.size) {
      var elf = 0
      while (i < input.size && input(i).nonEmpty) {
        elf += input(i).toInt
        i += 1
      }
      elves = elves :+ elf
      i += 1
    }
    elves.max
  }

  def part2(input: Seq[String]): Int = {
    var i = 0
    var elves = Seq[Int]()
    while (i < input.size) {
      var elf = 0
      while (i < input.size && input(i).nonEmpty) {
        elf += input(i).toInt
        i += 1
      }
      elves = elves :+ elf
      i += 1
    }
    elves.sorted.reverse.take(3).sum
  }
}
