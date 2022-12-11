package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day11Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day11.part1(getInput("day11/test0.txt")), 10605L)
  }

  test("Part 1") {
    assertEquals(Day11.part1(getInput("day11/input.txt")), 107822L)
  }

  test("Part 2") {
    assertEquals(Day11.part2(getInput("day11/test0.txt")), 2713310158L)
  }

  test("Part 2") {
    assertEquals(Day11.part2(getInput("day11/input.txt")), 27267163742L)
  }

}
