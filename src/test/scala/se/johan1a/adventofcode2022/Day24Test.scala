package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day24Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day24.part1(getInput("day24/test0.txt")), 18)
  }

  // 177 too low
  // 179 too low
  test("Part 1") {
    assertEquals(Day24.part1(getInput("day24/input.txt")), 1)
  }

  test("Part 2") {
    assertEquals(Day24.part2(getInput("day24/input.txt")), -1)
  }

}
