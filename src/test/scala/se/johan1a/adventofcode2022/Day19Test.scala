package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day19Test extends munit.FunSuite {

   test("Part 1 test ") {
     assertEquals(Day19.part1(getInput("day19/test0.txt")), 33)
   }

  test("Part 1 test ") {
    assertEquals(Day19.part1(getInput("day19/test1.txt")), 9)
  }

   test("Part 1") {
     assertEquals(Day19.part1(getInput("day19/input.txt")), 1192)
   }

  test("Part 2") {
    assertEquals(Day19.part2(getInput("day19/input.txt")), -1)
  }

}
