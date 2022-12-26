package se.johan1a.adventofcode2022

import Utils._
import scala.io.StdIn.readLine
import scala.collection.mutable.{Queue, Map}

object Day24 {

  var i = 0
  var seen = Map[(Vec2, Int), Int]()

  val large = 1000000000
  var best = large

  def part1(input: Seq[String]): Int = {
    seen = Map()
    best = large
    i = 0
    val blizzards = parse(input)

    //blizzards.foreach{println}
    var pos = Vec2(1, 0)
    val min = Vec2(1, 1)
    val max = Vec2(input.head.size, input.size)
    var target = Vec2(input.head.size - 2, input.size - 1)

    val width = max.x - 2
    val height = max.y - 2

    val r = shortest(blizzards, pos, target, max)
    println((min, max, target, seen.size))

    r._1
  }

  def shortest(
      startBlizzards: Seq[Blizzard],
      startPos: Vec2,
      target: Vec2,
      maxPos: Vec2
  ): (Int, Seq[Vec2]) = {
    val queue = Queue[(Vec2, Int)]((startPos, 0))
    seen = seen + ((startPos, 0) -> 0)

    val width = maxPos.x - 2
    val height = maxPos.y - 2
    val lcm = getLcm(Seq(width.toInt, height.toInt))
    println(s"width:$width, height:$height, lcm: $lcm")

    while (queue.nonEmpty) {
      val (pos, minutes) = queue.dequeue()

      val state = (pos, minutes % lcm)

      assert(i < 1000000)
      i += 1
      if (i % 1000 == 0) {
        println(s"i: $i, queue.size: ${queue.size}, seen.size: ${seen.size}")
      }

      if (pos == target) {
        println(s"reached target after $minutes min")
        best = Math.min(best, minutes)
        return (best, Seq.empty)
      } else {
        var newPositions = neighbors(
          pos,
          min = Vec2(1, 1),
          max = maxPos,
          includeDiagonals = false
        ) :+ pos
        if (pos == Vec2(target.x, target.y - 1)) {
          newPositions = target +: newPositions
        }

        var blizzards = startBlizzards

        0.until(minutes % lcm).foreach { _ =>
          blizzards = moveBlizzards(blizzards, maxPos)
        }
        val blizzardPositions = blizzards.map(_.pos).toSet
        newPositions = newPositions.filterNot { p =>
          blizzardPositions.contains(p)
        }

        if (false) {
          printMap(blizzards, pos, target, maxPos)
          println()
          println(state)
          readLine()
        }

        val nn =
          newPositions.map(p => (p, minutes + 1)).filter { case (newP, newM) =>
            val newState = (newP, newM % lcm)
            val r = !seen.contains(newState)
            seen = seen + (newState -> newM)
            r
          }

        queue.enqueueAll(nn)
      }
    }

    (best, Seq.empty)
  }

  def moveBlizzards(blizzards: Seq[Blizzard], maxPos: Vec2) = {
    blizzards.map { blizzard =>
      var bPos = blizzard.pos
      blizzard.dir match {
        case '>' =>
          bPos = add(Vec2(1, 0), blizzard.pos)
          if (bPos.x == maxPos.x - 1) {
            bPos = Vec2(1, bPos.y)
          }
        case '<' =>
          bPos = add(Vec2(-1, 0), blizzard.pos)
          if (bPos.x == 0) {
            bPos = Vec2(maxPos.x - 2, bPos.y)
          }
        case 'v' =>
          bPos = add(Vec2(0, 1), blizzard.pos)
          if (bPos.y == maxPos.y - 1) {
            bPos = Vec2(bPos.x, 1)
          }
        case '^' =>
          bPos = add(Vec2(0, -1), blizzard.pos)
          if (bPos.y == 0) {
            bPos = Vec2(bPos.x, maxPos.y - 2)
          }
      }
      Blizzard(blizzard.dir, bPos)
    }
  }

  def printMap(
      blizzards: Seq[Blizzard],
      pos: Vec2,
      target: Vec2,
      maxPos: Vec2
  ) = {
    val min = Vec2(1, 1)
    (min.x - 1).until(maxPos.x).map { x =>
      print('#')
    }
    println()
    min.y.until(maxPos.y - 1).map { y =>
      print('#')
      min.x.until(maxPos.x - 1).map { x =>
        val p = Vec2(x, y)
        if (pos == p) {
          print('P')
        } else {
          val b = blizzards.filter(_.pos == p)
          if (b.size == 1) {
            print(b.head.dir)
          } else if (b.isEmpty) {
            print('.')
          } else {
            print(b.size)
          }
        }
      }
      print('#')
      println()
    }
    (min.x - 1).until(maxPos.x).map { y =>
      print('#')
    }
    println()
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  case class Blizzard(dir: Char, pos: Vec2)

  def parse(input: Seq[String]): Seq[Blizzard] = {
    var blizzards = Seq[Blizzard]()
    input.indices.foreach { y =>
      input.head.indices.foreach { x =>
        if (Set('>', '<', '^', 'v').contains(input(y).charAt(x))) {
          blizzards = blizzards :+ Blizzard(input(y).charAt(x), Vec2(x, y))
        }
      }
    }
    blizzards
  }

  def getLcm(list: Seq[Int]): Int = list.foldLeft(1: Int) { (a, b) =>
    b * a / Stream
      .iterate((a, b)) { case (x, y) => (y, x % y) }
      .dropWhile(_._2 != 0)
      .head
      ._1
      .abs
  }
}
