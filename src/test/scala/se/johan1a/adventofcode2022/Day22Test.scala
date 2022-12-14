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

  // From F and back
  test("Part 2 test: F -> U -> F") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(50, 0),
        testPath = "L1RR1"
      ),
      (Vec2(50, 0), 1)
    )
  }

  test("Part 2 test: F -> L -> F") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(50, 0),
        testPath = "LL1LL1"
      ),
      (Vec2(50, 0), 0)
    )
  }

  test("Part 2 test: F -> T -> F") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(50, 49),
        testPath = "R1LL1"
      ),
      (Vec2(50, 49), 3)
    )
  }

  test("Part 2 test: F -> R -> F") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(99, 98),
        testPath = "1LL1"
      ),
      (Vec2(99, 98), 2)
    )
  }

  // From T and back
  test("Part 2 test: T -> L -> T") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(50, 50),
        testPath = "RR1RR1"
      ),
      (Vec2(50, 50), 0)
    )
  }

  // From T and back
  test("Part 2 test: T -> R -> T") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(99, 50),
        testPath = "1LL1"
      ),
      (Vec2(99, 50), 2)
    )
  }

  test("Part 2 test: T -> F -> T") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(98, 50),
        testPath = "L1LL1"
      ),
      (Vec2(98, 50), 1)
    )
  }

  test("Part 2 test: T -> B -> T") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(98, 99),
        testPath = "R1LL1"
      ),
      (Vec2(98, 99), 3)
    )
  }

  // From B and back
  test("Part 2 test: B -> T -> B") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(50, 100),
        testPath = "R1RR1"
      ),
      (Vec2(50, 100), 3)
    )
  }

  test("Part 2 test: B -> T -> B") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(98, 100),
        testPath = "L1RR1"
      ),
      (Vec2(98, 100), 1)
    )
  }

  test("Part 2 test: B -> L -> B") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(50, 100),
        testPath = "LL1RR1"
      ),
      (Vec2(50, 100), 0)
    )
  }

  test("Part 2 test: B -> U -> B") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(50, 100),
        testPath = "LL1RR1"
      ),
      (Vec2(50, 100), 0)
    )
  }

  test("Part 2 test: B -> R -> B") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(50, 100),
        testPath = "1RR1"
      ),
      (Vec2(50, 100), 2)
    )
  }

  // test from R
  test("Part 2 test: R -> F -> R") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(100, 0),
        testPath = "RR1RR1"
      ),
      (Vec2(100, 0), 0)
    )
  }

  test("Part 2 test: R -> B -> R") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(149, 1),
        testPath = "1RR1"
      ),
      (Vec2(149, 1), 2)
    )
  }

  test("Part 2 test: R -> B -> R (higher y)") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(149, 49),
        testPath = "1RR1"
      ),
      (Vec2(149, 49), 2)
    )
  }

  test("Part 2 test: R -> U -> R") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(100, 0),
        testPath = "L1RR1"
      ),
      (Vec2(100, 0), 1)
    )
  }

  test("Part 2 test: R -> U -> R (higher x)") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(148, 0),
        testPath = "L1RR1"
      ),
      (Vec2(148, 0), 1)
    )
  }

  test("Part 2 test: R -> T -> R") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(100, 49),
        testPath = "R1RR1"
      ),
      (Vec2(100, 49), 3)
    )
  }

  // test from L
  test("Part 2 test: L -> T -> L") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(49, 100),
        testPath = "L1RR1"
      ),
      (Vec2(49, 100), 1)
    )
  }

  test("Part 2 test: L -> F -> L") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(0, 100),
        testPath = "LL1RR1",
        doOutput = false,
        isManual = false
      ),
      (Vec2(0, 100), 0)
    )
  }

  test("Part 2 test: L -> B -> L") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(49, 149),
        testPath = "1RR1"
      ),
      (Vec2(49, 149), 2)
    )
  }

  test("Part 2 test: L -> U -> L") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(49, 149),
        testPath = "R1RR1"
      ),
      (Vec2(49, 149), 3)
    )
  }

  // test from U
  test("Part 2 test: U -> L -> U") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(0, 150),
        testPath = "L1RR1"
      ),
      (Vec2(0, 150), 1)
    )
  }

  test("Part 2 test: U -> F -> U") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(0, 150),
        testPath = "LL1RR1"
      ),
      (Vec2(0, 150), 0)
    )
  }

  test("Part 2 test: U -> B -> U") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(49, 199),
        testPath = "1RR1"
      ),
      (Vec2(49, 199), 2)
    )
  }

  test("Part 2 test: U -> R -> U") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(47, 199),
        testPath = "R1RR1"
      ),
      (Vec2(47, 199), 3)
    )
  }

  test("Corner U B R U") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(47, 198),
        testPath = "4R4R4LL4L4L4",
        doOutput = false,
        isManual = false
      ),
      (Vec2(47, 198), 2)
    )
  }

  test("Corner U L B U") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(49, 150),
        testPath = "L1R1R1RR1L1L1",
        doOutput = false,
        isManual = false
      ),
      (Vec2(49, 150), 1)
    )
  }

  test("Corner U L F U") {
    assertEquals(
      Day22.part2(
        getInput("day22/input.txt"),
        startPos = Vec2(0, 150),
        testPath = "L1L1L1LL1R1R1R",
        doOutput = false,
        isManual = false
      ),
      (Vec2(0, 150), 2)
    )
  }

  test("Part 2") {
    val (pos, dir) = Day22.part2(getInput("day22/input.txt"), doOutput = false)
    val result = 1000 * (pos.y.toInt + 1) + 4 * (pos.x.toInt + 1) + dir
    assertEquals(result, 55267)
  }

}
