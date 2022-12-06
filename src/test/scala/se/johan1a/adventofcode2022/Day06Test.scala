package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day06Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day06.part1(getInput("day06/test0.txt").head), 7)
  }

  test("Part 1") {
    assertEquals(Day06.part1(getInput("day06/input.txt").head), 1356)
  }

  test("Part 2 test") {
    assertEquals(Day06.part2(getInput("day06/test0.txt").head), 19)
  }

  test("Part 2") {
    assertEquals(Day06.part2(getInput("day06/input.txt").head), 2564)
  }
}
