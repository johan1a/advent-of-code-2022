package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day20Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day20.part1(getInput("day20/test0.txt")), 3)
  }

  test("Part 1 test 1") {
    assertEquals(Day20.part1(getInput("day20/test1.txt")), 3)
  }

  //  // 8428 too low
  test("Part 1") {
    assertEquals(Day20.part1(getInput("day20/input.txt")), 27726)
  }

  test("Part 2") {
    assertEquals(Day20.part2(getInput("day20/input.txt")), -1)
  }

}
