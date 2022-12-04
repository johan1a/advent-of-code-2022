package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day03Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day03.part1(getInput("day03/test0.txt")), 157)
  }

  test("Part 1") {
    assertEquals(Day03.part1(getInput("day03/input.txt")), -1)
  }

  // test("Part 2") {
  //   assertEquals(Day03.part2(getInput("day03/input.txt")), -1)
  // }

}
