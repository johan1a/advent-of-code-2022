package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day06Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day06.part1(getInput("day06/test0.txt")), 7)
  }

  test("Part 1") {
    assertEquals(Day06.part1(getInput("day06/input.txt")), 1356)
  }

  test("Part 2 test") {
    assertEquals(Day06.part2(getInput("day06/test0.txt")), 19)
  }

  test("Part 2") {
    assertEquals(Day06.part2(getInput("day06/input.txt")), 2564)
  }
}
