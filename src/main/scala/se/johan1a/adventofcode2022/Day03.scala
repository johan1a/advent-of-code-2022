package se.johan1a.adventofcode2022

object Day03 {

  def part1(input: Seq[String]): Int = {
    input.map(line => {
      val a = line.take(line.size / 2).toSet
      val b = line.drop(line.size / 2).toSet
      val common = a.intersect(b).head.toString
      println(common)
      priority(common)
    }).sum
  }

  private def priority(l: String): Int = {
    if (l >= "a" && l <= "z") {
      l.toCharArray().head.toInt - 'a'.toInt + 1
    } else if (l >= "A" && l <= "Z") {
      l.toCharArray().head.toInt - 'A'.toInt + 27
    } else {
      ???
    }
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
