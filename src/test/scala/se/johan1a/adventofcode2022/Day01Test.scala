package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day01Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day01.part1(getInput("day01/input.txt")), 72718)
  }

  test("Part 2") {
    assertEquals(Day01.part2(getInput("day01/input.txt")), 213089)
  }

}
