package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day18Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day18.part1(getInput("day18/test1.txt")), 10)
  }

  test("Part 1") {
    assertEquals(Day18.part1(getInput("day18/test0.txt")), 64)
  }

  test("Part 1") {
    assertEquals(Day18.part1(getInput("day18/input.txt")), 3396)
  }

  test("Part 2") {
    assertEquals(Day18.part2(getInput("day18/input.txt")), -1)
  }

}
