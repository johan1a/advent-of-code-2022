package se.johan1a.adventofcode2022

object Day01 {

  def part1(input: Seq[String]): Int = {
    parseElves(input).max
  }

  def part2(input: Seq[String]): Int = {
    parseElves(input).sorted.reverse.take(3).sum
  }

  private def parseElves(input: Seq[String]): Seq[Int] = {
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
    elves
  }
}
