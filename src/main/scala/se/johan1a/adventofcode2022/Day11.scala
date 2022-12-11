package se.johan1a.adventofcode2022

import Utils._

object Day11 {

  sealed trait Op
  case class Mul(n: Long) extends Op
  case class Add(n: Long) extends Op
  case class Pow2() extends Op

  case class Monkey(
      n: Int,
      var items: Seq[Long],
      operation: Op,
      divisibleTest: Long,
      ifTrueMonkey: Int,
      ifFalseMonkey: Int
  )

  def part1(input: Seq[String], rounds: Int = 20): Long = {
    solve(input, 20, x => x / 3)
  }

  def part2(input: Seq[String]): Long = {
    solve(input, 10000)
  }

  def solve(
      input: Seq[String],
      nbrRounds: Int,
      f: (Long => Long) = (x => x)
  ): Long = {
    val monkeys = split(input).map(parse2)

    var totalInspected: Map[Int, Long] = Map[Int, Long]().withDefaultValue(0L)
    val period = monkeys.map(_.divisibleTest).product

    0.until(nbrRounds).foreach { _ =>
      0.until(monkeys.size).map { i =>
        val monkey = monkeys(i)
        var nbrInspected = 0
        while (monkey.items.nonEmpty) {
          val item = monkey.items.head
          monkey.items = monkey.items.drop(1)
          val newValue = f(eval(monkey.operation, item)) % period
          val targetMonkey = if (newValue % monkey.divisibleTest == 0) {
            monkeys(monkey.ifTrueMonkey)
          } else {
            monkeys(monkey.ifFalseMonkey)
          }
          targetMonkey.items = targetMonkey.items :+ newValue
          nbrInspected += 1
        }
        totalInspected =
          totalInspected.updated(i, totalInspected(i) + nbrInspected.toLong)
      }
    }

    totalInspected.toSeq
      .sortBy { entry =>
        entry._2
      }
      .drop(totalInspected.size - 2)
      .map(_._2)
      .product
  }

  private def eval(op: Op, num: Long): Long = {
    op match {
      case Mul(n) => num * n
      case Add(n) => num + n
      case Pow2() => num * num
    }
  }

  private def parse2(lines: Seq[String]): Monkey = {
    val n = lines.head match {
      case s"Monkey $i:" => i.toInt
    }
    val startingItems = numbers(lines(1))
    val operation = lines(2) match {
      case s"  Operation: new = old * old" => Pow2()
      case s"  Operation: new = old * $x"  => Mul(x.toLong)
      case s"  Operation: new = old + $x"  => Add(x.toLong)
    }
    val test = lines(3) match {
      case s"  Test: divisible by $x" => x.toLong
    }
    val ifTrue = numbers(lines(4)).head.toInt
    val ifFalse = numbers(lines(5)).head.toInt

    Monkey(n, startingItems, operation, test, ifTrue, ifFalse)
  }
}
