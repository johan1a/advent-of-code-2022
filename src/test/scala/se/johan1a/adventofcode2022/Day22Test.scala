package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput
import se.johan1a.adventofcode2022.Day22._

class Day22Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day22.part1(getInput("day22/test0.txt")), 6032)
  }

  test("Part 1") {
    assertEquals(Day22.part1(getInput("day22/input.txt")), 162186)
  }

  test("Part 2 test") {
    val order =
      Seq(
        InputSide(Front, 0, 0, 0),
        InputSide(Bottom, 2, 2, 180),
        InputSide(Left, 0, 0, 0),
        InputSide(Top, 0, 0, 0),
        InputSide(Back, 0, 0, 0),
        InputSide(Right, 0, -1, 270)
      )
    assertEquals(Day22.part2(getInput("day22/test0.txt"), order, 4, 2), -1)
  }

}
