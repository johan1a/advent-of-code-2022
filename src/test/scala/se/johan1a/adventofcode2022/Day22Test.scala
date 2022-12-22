package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day22Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day22.part1(getInput("day22/test0.txt")), 6032)
  }

  test("Part 1") {
    assertEquals(Day22.part1(getInput("day22/input.txt")), 162186)
  }

  test("Part 2") {
    assertEquals(Day22.part2(getInput("day22/input.txt")), -1)
  }

}
