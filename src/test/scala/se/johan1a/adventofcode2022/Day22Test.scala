package se.johan1a.adventofcode2022

import se.johan1a.adventofcode2022.TestInputUtil.getInput
import se.johan1a.adventofcode2022.Day22._
import se.johan1a.adventofcode2022.Utils._

import scala.collection.mutable

class Day22Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day22.part1(getInput("day22/test0.txt")), 6032)
  }

  test("Part 1") {
    assertEquals(Day22.part1(getInput("day22/input.txt")), 162186)
  }

  test("Part 2 rotation test") {
    val grid = mutable.Map(
      Vec2(0,0)->'#', Vec2(1,0) -> '.',
      Vec2(0,1)->'.', Vec2(1,1) -> '#',
      )
    val actual: String = gridString(rotate(grid,90, 2))
    val expected: String = ".#\n#.\n".toString
    assertEquals(actual, expected)
  }

  test("Part 2 rotation test 2") {
    val grid = mutable.Map(
      Vec2(0,1) ->'#', Vec2(1,1) -> '.',
      Vec2(0,2) ->'.', Vec2(1,2) -> '#',
    )
    val actual: String = gridString(rotate(grid,90, 2))
    val expected: String = ".#\n#.\n".toString
    println((actual,expected))
    assertEquals(actual, expected)
  }

  // test("Part 2 test") {
  //   val order =
  //     Seq(
  //       InputSide(Front, 0, 0, 0),
  //       InputSide(Bottom, 2, 2, 180),
  //       InputSide(Left, 0, 0, 0),
  //       InputSide(Top, 0, 0, 0),
  //       InputSide(Back, 0, 0, 0),
  //       InputSide(Right, 0, -1, 270)
  //     )
  //   assertEquals(Day22.part2(getInput("day22/test0.txt"), order, 4, 2), -1)
  // }

}
