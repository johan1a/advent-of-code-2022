package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day13Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day13.part1(getInput("day13/test1.txt")), 1)
  }

  test("Part 1") {
    assertEquals(Day13.part1(getInput("day13/test0.txt")), 13)
  }

  // 5532 too high
  // 3898 too low
  test("Part 1") {
    assertEquals(Day13.part1(getInput("day13/input.txt")), 5196)
  }

  test("Part 2") {
    assertEquals(Day13.part2(getInput("day13/input.txt")), -1)
  }

}
