package se.johan1a.adventofcode2022

object Day21 {

  sealed trait Op
  case class Add(a: String, b: String) extends Op
  case class Sub(a: String, b: String) extends Op
  case class Mul(a: String, b: String) extends Op
  case class Div(a: String, b: String) extends Op
  case class Num(value: Long) extends Op

  def part1(input: Seq[String]): Long = {
    val instructions = input.map(parse).toMap
    eval(instructions, "root")
  }

  def part2(input: Seq[String]): Long = {
    -1
  }

  private def eval(instructions: Map[String, Op], id: String): Long = {
    instructions(id) match {
      case Num(value) => value
      case Add(a, b)  => eval(instructions, a) + eval(instructions, b)
      case Sub(a, b)  => eval(instructions, a) - eval(instructions, b)
      case Mul(a, b)  => eval(instructions, a) * eval(instructions, b)
      case Div(a, b)  => eval(instructions, a) / eval(instructions, b)
    }
  }

  private def parse(line: String): (String, Op) = {
    line match {
      case s"$id: $a + $b" => id -> Add(a, b)
      case s"$id: $a - $b" => id -> Sub(a, b)
      case s"$id: $a * $b" => id -> Mul(a, b)
      case s"$id: $a / $b" => id -> Div(a, b)
      case s"$id: $n"      => id -> Num(n.toLong)
    }
  }
}
