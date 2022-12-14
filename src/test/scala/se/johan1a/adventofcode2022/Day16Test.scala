package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day16Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day16.part1(getInput("day16/test0.txt")), 1651)
  }

  test("Part 1") {
    assertEquals(Day16.part1(getInput("day16/input.txt")), 1584)
  }

  test("Part 2") {
    assertEquals(Day16.part2(getInput("day16/test0.txt")), 1707)
  }

  test("Part 2") {
    assertEquals(Day16.part2(getInput("day16/input.txt")), 2052)
  }

}
