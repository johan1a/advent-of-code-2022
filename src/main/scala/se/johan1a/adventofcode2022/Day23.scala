package se.johan1a.adventofcode2022
import Utils._
import scala.collection.mutable.Map
import scala.io.StdIn.readLine

object Day23 {

  case class Elf(var dirI: Int, var proposedTarget: Option[Vec2])

  val directions = Seq(
    Vec2(0, -1),
    Vec2(0, 1),
    Vec2(-1, 0),
    Vec2(1, 0)
  )

  val offsetList = Seq(
    Seq(Vec2(0, -1), Vec2(1, -1), Vec2(-1, -1)),
    Seq(Vec2(0, 1), Vec2(1, 1), Vec2(-1, 1)),
    Seq(Vec2(-1, 0), Vec2(-1, -1), Vec2(-1, 1)),
    Seq(Vec2(1, 0), Vec2(1, -1), Vec2(1, 1))
  )

  def part1(input: Seq[String]): Int = {
    var elves = parse(input)
    0.until(10).foreach { _ =>
      proposeTargets(elves)
      elves = moveElves(elves)
    }
    val minX: Long = elves.keys.minBy(_.x).x
    val minY: Long = elves.keys.minBy(_.y).y
    val maxX: Long = elves.keys.maxBy(_.x).x
    val maxY: Long = elves.keys.maxBy(_.y).y
    ((maxX - minX + 1) * (maxY - minY + 1) - elves.size).toInt
  }

  def part2(input: Seq[String]): Int = {
    var elves = parse(input)
    var shouldContinue = true
    var round = 1
    while (shouldContinue) {
      proposeTargets(elves)
      val newElves = moveElves(elves)
      if (elves.keys == newElves.keys) {
        shouldContinue = false
      }
      elves = newElves
      round += 1
    }
    round - 1
  }

  def printElves(elves: Map[Vec2, Elf], d: Int = 3) = {
    val minX: Long = elves.keys.minBy(_.x).x
    val minY: Long = elves.keys.minBy(_.y).y
    val maxX: Long = elves.keys.maxBy(_.x).x
    val maxY: Long = elves.keys.maxBy(_.y).y
    val d = 3
    (minY - d).until(maxY + 3).foreach { y =>
      (minX - d).until(maxX + 3).foreach { x =>
        val c = elves.getOrElse(Vec2(x, y), '.') match {
          case '.' => '.'
          case _   => '#'
        }
        print(c)
      }
      println()
    }
  }

  private def proposeTargets(elves: Map[Vec2, Elf]) = {
    elves.foreach { case (pos, elf) =>
      val nn = neighbors(pos, includeDiagonals = true)
      if (nn.exists(elves.contains)) {

        var i = 0
        while (i < 4 && elf.proposedTarget.isEmpty) {
          val offsets = offsetList((elf.dirI + i) % 4)
          if (offsets.forall(o => !elves.contains(add(pos, o)))) {
            elf.proposedTarget = Some(add(pos, directions((elf.dirI + i) % 4)))
          }

          i = i + 1
        }
      }
    }
  }

  private def getPosCounts(elves: Map[Vec2, Elf]) = elves
    .filter(_._2.proposedTarget.isDefined)
    .map { case (pos, elf) =>
      elf.proposedTarget.get
    }
    .groupBy(identity)
    .mapValues(_.size)
    .toMap

  private def moveElves(elves: Map[Vec2, Elf]): Map[Vec2, Elf] = {
    val posCounts = getPosCounts(elves)
    val newElves = Map[Vec2, Elf]()
    elves.foreach { case (pos, elf) =>
      elf.proposedTarget match {
        case None =>
          newElves(pos) = elf
        case Some(proposed) =>
          if (posCounts.getOrElse(proposed, 0) == 1) {
            newElves(proposed) = elf
          } else {
            newElves(pos) = elf
          }
      }
      elf.proposedTarget = None
      elf.dirI = elf.dirI + 1
    }
    newElves
  }

  private def parse(input: Seq[String]) = {
    val elves = Map[Vec2, Elf]()
    input.indices.foreach { y =>
      input.head.indices.foreach { x =>
        if (input(y).charAt(x) == '#') {
          elves += (Vec2(x, y) -> Elf(0, None))
        }
      }
    }
    elves
  }
}
