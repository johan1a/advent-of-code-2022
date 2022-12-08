package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day08Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day08.part1(getInput("day08/test0.txt")), 21)
  }

  test("Part 1") {
    assertEquals(Day08.part1(getInput("day08/input.txt")), 1708)
  }

  test("Part 2 test") {
    assertEquals(Day08.part2(getInput("day08/test0.txt")), 8)
  }

  test("Part 2") {
    assertEquals(Day08.part2(getInput("day08/input.txt")), 504000)
  }

}
