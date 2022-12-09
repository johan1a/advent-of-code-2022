package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day09Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day09.part1(getInput("day09/test0.txt")), 13)
  }

  test("Part 1") {
    assertEquals(Day09.part1(getInput("day09/input.txt")), 6011)
  }

  test("Part 2 test") {
    assertEquals(Day09.part2(getInput("day09/test0.txt")), 1)
  }

  test("Part 2 test") {
    assertEquals(Day09.part2(getInput("day09/test1.txt")), 36)
  }

  test("Part 2") {
    assertEquals(Day09.part2(getInput("day09/input.txt")), 2419)
  }

}
