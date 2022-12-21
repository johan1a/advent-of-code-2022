package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day21Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day21.part1(getInput("day21/test0.txt")), 152L)
  }

  test("Part 1") {
    assertEquals(Day21.part1(getInput("day21/input.txt")), 56490240862410L)
  }

  test("Part 2") {
    assertEquals(Day21.part2(getInput("day21/input.txt")), -1L)
  }

}
