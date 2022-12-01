package se.johan1a.adventofcode2022

object Day01 {

  def part1(input: Seq[String]): Int = {
    parseElves(input).max
  }

  def part2(input: Seq[String]): Int = {
    parseElves(input).sorted.reverse.take(3).sum
  }

  private def parseElves(input: Seq[String]): Seq[Int] = {
    Utils.split(input).map(elf => elf.map(_.toInt).sum)
  }
}
