package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day15Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day15.part1(getInput("day15/test0.txt"), 10), 26)
  }

  test("Part 1") {
    assertEquals(Day15.part1(getInput("day15/input.txt"), 2000000), 6078701)
  }

  test("Part 2") {
    assertEquals(Day15.part2(getInput("day15/test0.txt"), 20, 20), 56000011L)
  }

  test("Part 2") {
    assertEquals(
      Day15.part2(getInput("day15/input.txt"), 4000000, 4000000),
      12567351400528L
    )
  }

}
