package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day05Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day05.part1(getInput("day05/test0.txt")), "CMZ")
  }

  test("Part 1") {
    assertEquals(Day05.part1(getInput("day05/input.txt")), "")
  }

//   test("Part 2") {
//     assertEquals(Day05.part2(getInput("day05/input.txt")), -1)
//   }

}
