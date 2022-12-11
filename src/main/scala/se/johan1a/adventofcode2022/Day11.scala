package se.johan1a.adventofcode2022

import Utils._

object Day11 {

  sealed trait Op
  case class Mul(n: Long) extends Op
  case class Add(n: Long) extends Op
  case class Pow2() extends Op

  case class Item(id: Int, var value: Long)

  case class Monkey(n: Int, var items: Seq[Long], operation: Op, divisibleTest: Long, ifTrueMonkey: Int, ifFalseMonkey: Int)
  case class Monkey2(n: Int, var items: Seq[Item], operation: Op, divisibleTest: Long, ifTrueMonkey: Int, ifFalseMonkey: Int)

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
    val monkeys = split(input).map(parse2)
    monkeys.foreach(println)

    var totalInspected: Map[Int, Long] = Map[Int, Long]().withDefaultValue(0L)
    var history = Map[Int, Seq[Int]]().withDefaultValue(Seq())

    val period = monkeys.map(_.divisibleTest).product

    0.until(10000).foreach { _ =>
      0.until(monkeys.size).map { i =>
        val monkey = monkeys(i)
        var nbrInspected = 0
        while(monkey.items.nonEmpty) {
          val item = monkey.items.head
          //history = history.updated(item.id, history(item.id) :+ i)
          monkey.items = monkey.items.drop(1)
          val newValue = eval(monkey.operation, item.value) % period
          //println(s"${item.id}: ${history(item.id).mkString(",")}")
          //println(s"monkey ${monkey.n}, $item, ${monkey.operation}, new: $newValue, test: ${monkey.divisibleTest}")
          assert(newValue >= 0)
          val targetMonkey = if (newValue % monkey.divisibleTest == 0) {
            monkeys(monkey.ifTrueMonkey)
          } else {
            monkeys(monkey.ifFalseMonkey)
          }
          targetMonkey.items = targetMonkey.items :+ Item(item.id, newValue)
          nbrInspected += 1
        }
        totalInspected = totalInspected.updated(i, totalInspected(i) + nbrInspected.toLong)
      }
    }

    totalInspected.toSeq.sortBy { entry =>
      entry._2
    }.drop(totalInspected.size - 2).map(_._2).product
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

  private def parse2(lines: Seq[String]): Monkey2 = {
    val n = lines.head match {
      case s"Monkey $i:" => i.toInt
    }
    val startingItems = numbers(lines(1)).map(n => Item(n.toInt, n))
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

    Monkey2(n, startingItems, operation, test, ifTrue, ifFalse)
  }
}
