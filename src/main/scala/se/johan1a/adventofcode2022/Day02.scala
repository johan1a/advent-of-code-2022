package se.johan1a.adventofcode2022

object Day02 {

  //score 1 rock 2 paper 3 scissors

  def part1(input: Seq[String]): Int = {
    input
      .map(line => {
        val split = line.split(" ")
        val them = split.head.toString
        val me = split.last.toString
        val shapeScore = me match {
          case "X" => 1 // Rock
          case "Y" => 2 // paper
          case "Z" => 3 // scissors
        }
        // a rock, b paper, c scissor
        val winScore = (me, them) match {
          case ("X", "A") => 3
          case ("Y", "B") => 3
          case ("Z", "C") => 3
          case ("X", "C") => 6
          case ("Y", "A") => 6
          case ("Z", "B") => 6
          case (_, _)     => 0
        }
        shapeScore + winScore
      })
      .sum
  }

  def part2(input: Seq[String]): Int = {
    val required = Map(
      "A" -> Map("W" -> "Y", "L" -> "Z", "D" -> "X"),
      "B" -> Map("W" -> "Z", "L" -> "X", "D" -> "Y"),
      "C" -> Map("W" -> "X", "L" -> "Y", "D" -> "Z")
    )
    input
      .map(line => {
        val split = line.split(" ")
        val them = split.head.toString
        val outcome = split.last.toString
          .replace("X", "L")
          .replace("Y", "D")
          .replace("Z", "W")
        val me = required(them)(outcome)
        val shapeScore = me match {
          case "X" => 1 // Rock
          case "Y" => 2 // paper
          case "Z" => 3 // scissors
        }
        // a rock, b paper, c scissor
        val winScore = (me, them) match {
          case ("X", "A") => 3
          case ("Y", "B") => 3
          case ("Z", "C") => 3
          case ("X", "C") => 6
          case ("Y", "A") => 6
          case ("Z", "B") => 6
          case (_, _)     => 0
        }
        shapeScore + winScore
        shapeScore + winScore
      })
      .sum
  }
}
