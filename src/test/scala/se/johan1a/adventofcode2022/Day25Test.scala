package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day25Test extends munit.FunSuite {

  // test("Part 1 test") {
  //   assertEquals(Day25.part1(getInput("day25/test0.txt")), "2=-1=0")
  // }

  test("Part 1 test") {
    assertEquals(Day25.toSnafu(15L), "1=0")
  }

  // test("Part 1 test") {
  //   assertEquals(Day25.toSnafu(13L), "1==")
  // }

  // test("Part 1 test") {
  //   assertEquals(Day25.fromSnafu("1===="), 1L)
  // }

  // test("Part 1 test") {
  //   assertEquals(Day25.toSnafu(10057560154L), "")
  // }

  // test("Part 1") {
  //   assertEquals(Day25.part1(getInput("day25/input.txt")), "0")
  // }

  test("Part 2") {
    assertEquals(Day25.part2(getInput("day25/input.txt")), -1)
  }

}
