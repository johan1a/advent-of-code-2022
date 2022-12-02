package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day02Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day02.part1(getInput("day02/test0.txt")), 15)
  }

  test("Part 1") {
    assertEquals(Day02.part1(getInput("day02/input.txt")), 12276)
  }

  test("Part 2") {
    assertEquals(Day02.part2(getInput("day02/test0.txt")), 12)
  }

  test("Part 2") {
    assertEquals(Day02.part2(getInput("day02/input.txt")), 9975)
  }

}
