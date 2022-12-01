package se.johan1a.adventofcode2022

import Utils._

class UtilsTest extends munit.FunSuite {

  test("split") {
    val input = Seq("", "a", "", "b", "c", "", "", "d", " ", "e")

    val expected = Seq(Seq("a"), Seq("b", "c"), Seq("d"), Seq("e"))

    assertEquals(split(input), expected)
  }

  test("add Vec2") {
    val a = Vec2(1, 2)
    val b = Vec2(3, 4)

    assertEquals(add(a, b), Vec2(4, 6))
  }

  test("sub Vec2") {
    val a = Vec2(1, 2)
    val b = Vec2(3, 4)

    assertEquals(sub(a, b), Vec2(-2, -2))
  }

  test("add Vec3") {
    val a = Vec3(1, 2, -2)
    val b = Vec3(3, 4, -1)

    assertEquals(add(a, b), Vec3(4, 6, -3))
  }

  test("sub Vec3") {
    val a = Vec3(1, 2, 1)
    val b = Vec3(3, 4, -3)

    assertEquals(sub(a, b), Vec3(-2, -2, 4))
  }

}
