package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day07Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day07.part1(getInput("day07/test0.txt")), 95437L)
  }

  test("Part 1") {
    assertEquals(Day07.part1(getInput("day07/test1.txt")), 1000L)
  }

  // too low 1816826
  test("Part 1") {
    assertEquals(Day07.part1(getInput("day07/input.txt")), 1L)
  }

  test("Part 2") {
    assertEquals(Day07.part2(getInput("day07/input.txt")), -1)
  }

}
