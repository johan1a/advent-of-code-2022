package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day04Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day04.part1(getInput("day04/test0.txt")), 2)
  }

  test("Part 1") {
    assertEquals(Day04.part1(getInput("day04/input.txt")), 605)
  }

  test("Part 2 test") {
    assertEquals(Day04.part2(getInput("day04/test0.txt")), 4)
  }


  test("Part 2") {
    assertEquals(Day04.part2(getInput("day04/input.txt")), -1)
  }

}
