package se.johan1a.adventofcode2022

object Day02 {

  def part1(input: Seq[String]): Int = {
    input
      .map(line => {
        val split = line.split(" ")
        val them = abcToMove(split.head.toString)
        val me = xyzToMove(split.last.toString)
        shapeScore(me) + winScore(me, them)
      })
      .sum
  }

  def part2(input: Seq[String]): Int = {
    val requiredMove = Map(
      "R" -> Map("W" -> "P", "L" -> "S", "D" -> "R"),
      "P" -> Map("W" -> "S", "L" -> "R", "D" -> "P"),
      "S" -> Map("W" -> "R", "L" -> "P", "D" -> "S")
    )

    input
      .map(line => {
        val split = line.split(" ")

        val them = abcToMove(split.head.toString)
        val outcome = xyzToOutcome(split.last.toString)
        val me = requiredMove(them)(outcome)

        shapeScore(me) + winScore(me, them)
      })
      .sum
  }

  private def xyzToMove: String => String = {
    case "X" => "R"
    case "Y" => "P"
    case "Z" => "S"
  }

  private def xyzToOutcome: String => String = {
    case "X" => "L"
    case "Y" => "D"
    case "Z" => "W"
  }

  private def abcToMove: String => String = {
    case "A" => "R"
    case "B" => "P"
    case "C" => "S"
  }

  private def shapeScore: String => Int = {
    case "R" => 1
    case "P" => 2
    case "S" => 3
  }

  private def winScore: (String, String) => Int = {
    case ("R", "R") => 3
    case ("P", "P") => 3
    case ("S", "S") => 3
    case ("R", "S") => 6
    case ("P", "R") => 6
    case ("S", "P") => 6
    case (_, _)     => 0
  }
}
