package se.johan1a.adventofcode2022

import Utils._

object Day05 {

  def part1(input: Seq[String]): String = {
    solve(input, (a, b) => a ++ b.reverse)
  }

  def part2(input: Seq[String]): String = {
    solve(input, (a, b) => a ++ b)
  }

  private def solve(
      input: Seq[String],
      combine: (Seq[Char], Seq[Char]) => Seq[Char]
  ): String = {
    var (stacks, instructions) = parse(input)
    instructions.foreach(instruction => {
      val (n, src, dest) = (
        instruction(0).toInt,
        instruction(1).toInt - 1,
        instruction(2).toInt - 1
      )
      val sourceStack = stacks(src)
      val top = sourceStack.drop(sourceStack.size - n)
      stacks = stacks.updated(src, stacks(src).take(sourceStack.size - n))

      val destStack = stacks(dest)
      stacks = stacks.updated(dest, combine(destStack, top))
    })
    stacks.keys.toSeq.sorted
      .map(key => stacks(key).last)
      .mkString
  }

  private def parse(input: Seq[String]) = {
    var i = 0
    var stacks: Map[Int, Seq[Char]] = Map()
    while (!input(i).startsWith(" 1")) {
      var j = 0
      val line = input(i)

      while (j < input(i).size) {
        if (line.charAt(j) == '[') {
          val index = j / 4
          val stack = stacks.getOrElse(index, Seq())
          stacks = stacks.updated(index, line.charAt(j + 1) +: stack)
        }
        j += 1
      }
      i += 1
    }
    i += 2
    val instructions = input.drop(i).map(numbers)
    (stacks, instructions)
  }
}
