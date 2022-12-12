package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day12Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day12.part1(getInput("day12/test0.txt")), 31)
  }

  test("Part 1") {
    assertEquals(Day12.part1(getInput("day12/input.txt")), 383)
  }

  test("Part 2") {
    assertEquals(Day12.part2(getInput("day12/input.txt")), -1)
  }

}
