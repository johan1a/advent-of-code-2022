package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day17Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day17.part1(getInput("day17/test0.txt")), 3068L)
  }

  test("Part 1") {
    assertEquals(Day17.part1(getInput("day17/input.txt")), 3224L)
  }

  test("Part 2 test") {
    assertEquals(Day17.part2(getInput("day17/test0.txt")), 1514285714288L)
  }

  test("Part 2") {
    assertEquals(Day17.part2(getInput("day17/input.txt")), 1595988538691L)
  }

}
