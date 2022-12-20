package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day20Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day20.part1(getInput("day20/test0.txt")), 3L)
  }

  test("Part 1") {
    assertEquals(Day20.part1(getInput("day20/input.txt")), 27726L)
  }

  test("Part 2 test") {
    assertEquals(Day20.part2(getInput("day20/test0.txt")), 1623178306L)
  }
  
  test("Part 2") {
    assertEquals(Day20.part2(getInput("day20/input.txt")), 4275451658004L)
  }

}
