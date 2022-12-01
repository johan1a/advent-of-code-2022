package se.johan1a.adventofcode2022

class UtilsTest extends munit.FunSuite {

  test("split") {
    val input = Seq("", "a", "", "b", "c", "", "", "d", " ", "e")

    val expected = Seq(Seq("a"), Seq("b", "c"), Seq("d"), Seq("e"))

    assertEquals(Utils.split(input), expected)
  }

}
