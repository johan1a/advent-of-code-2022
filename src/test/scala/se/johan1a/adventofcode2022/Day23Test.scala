package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day23Test extends munit.FunSuite {

  test("Part 1 test 0") {
    assertEquals(Day23.part1(getInput("day23/test1.txt")), 25)
  }

  test("Part 1 test 1") {
    assertEquals(Day23.part1(getInput("day23/test0.txt")), 110)
  }

  test("Part 1") {
    assertEquals(Day23.part1(getInput("day23/input.txt")), 4000)
  }

  test("Part 2 test") {
    assertEquals(Day23.part2(getInput("day23/test0.txt")), 20)
  }

  test("Part 2") {
    assertEquals(Day23.part2(getInput("day23/input.txt")), 1040)
  }

}
