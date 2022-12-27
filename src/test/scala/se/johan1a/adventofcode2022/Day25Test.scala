package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput

class Day25Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day25.part1(getInput("day25/test0.txt")), "2=-1=0")
  }

  test("Part 1 test") {
    assertEquals(Day25.toSnafu(20L), "1-0")
  }

  test("Part 1 test") {
    assertEquals(Day25.toSnafu(10L), "20")
  }

  test("Part 1 test") {
    assertEquals(Day25.toSnafu(2022L), "1=11-2")
  }

  test("Part 1") {
    assertEquals(
      Day25.part1(getInput("day25/input.txt")),
      "20-=0=02=-21=00-02=2"
    )
  }

}
