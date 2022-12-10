package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day10Test extends munit.FunSuite {

  test("Part 1") {
    assertEquals(Day10.part1(getInput("day10/test1.txt")), 0)
  }

  test("Part 1") {
    assertEquals(Day10.part1(getInput("day10/test0.txt")), 13140)
  }

  test("Part 1") {
    assertEquals(Day10.part1(getInput("day10/input.txt")), 17840)
  }

  test("Part 2 test") {
    assertEquals(
      Day10.part2(getInput("day10/test0.txt")),
      getInput("day10/test0.output.txt")
    )
  }

  test("Part 2") {
    assertEquals(
      Day10.part2(getInput("day10/input.txt")),
      getInput("day10/input.output.txt")
    )
  }

}
