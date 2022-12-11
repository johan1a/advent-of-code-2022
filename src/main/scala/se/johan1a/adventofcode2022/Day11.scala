package se.johan1a.adventofcode2022

import Utils._

object Day11 {

  sealed trait Op
  case class Mul(n: Long) extends Op
  case class Add(n: Long) extends Op
  case class Pow2() extends Op

  case class Monkey(n: Int, var items: Seq[Long], operation: Op, divisibleTest: Long, ifTrueMonkey: Int, ifFalseMonkey: Int)

  def part1(input: Seq[String], rounds: Int = 20): Long = {
    val monkeys = split(input).map(parse)
    monkeys.foreach(println)

    var totalInspected: Map[Int, Long] = Map[Int, Long]().withDefaultValue(0L)
    0.until(20).foreach { _ =>
      0.until(monkeys.size).map { i =>
        val monkey = monkeys(i)
        var nbrInspected = 0
        while(monkey.items.nonEmpty) {
          val item = monkey.items.head
          monkey.items = monkey.items.drop(1)
          val newValue = eval(monkey.operation, item) / 3
          val targetMonkey = if (newValue % monkey.divisibleTest == 0) {
            monkeys(monkey.ifTrueMonkey)
          } else {
            monkeys(monkey.ifFalseMonkey)
          }
          targetMonkey.items = targetMonkey.items :+ newValue
          nbrInspected += 1
        }
        totalInspected = totalInspected.updated(i, totalInspected(i) + nbrInspected.toLong)
      }
    }

    totalInspected.toSeq.sortBy { entry =>
      entry._2
    }.drop(totalInspected.size - 2).map(_._2).product
  }

  def part2(input: Seq[String]): Long = {
    -1
  }

  private def eval(op: Op, num: Long): Long = {
    op match {
      case Mul(n) => num * n
      case Add(n) => num + n
      case Pow2() => num * num
    }
  }

  private def parse(lines: Seq[String]): Monkey = {
    val n = lines.head match {
      case s"Monkey $i:" => i.toInt
    }
    val startingItems = numbers(lines(1))
    val operation = lines(2) match {
      case s"  Operation: new = old * old" => Pow2()
      case s"  Operation: new = old * $x" => Mul(x.toLong)
      case s"  Operation: new = old + $x" => Add(x.toLong)
    }
    val test = lines(3) match {
      case s"  Test: divisible by $x" => x.toLong
    }
    val ifTrue = numbers(lines(4)).head.toInt
    val ifFalse = numbers(lines(5)).head.toInt

    Monkey(n, startingItems, operation, test, ifTrue, ifFalse)
  }
}
