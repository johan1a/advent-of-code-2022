package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day14Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day14.part1(getInput("day14/test0.txt")), 24)
  }

  test("Part 1") {
    assertEquals(Day14.part1(getInput("day14/input.txt")), 696)
  }

  test("Part 2") {
    assertEquals(Day14.part2(getInput("day14/test0.txt")), 93)
  }

  test("Part 2") {
    assertEquals(Day14.part2(getInput("day14/input.txt")), 23610)
  }

}
