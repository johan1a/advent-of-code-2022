package se.johan1a.adventofcode2022

import Utils._
import scala.io.StdIn.readLine

object Day24 {

  var i = 0
  var cache = Map[(Vec2, Int, Int), Int]()

  val large = 1000000000
  var best = large

  def part1(input: Seq[String]): Int = {
    cache = Map()
    best = large
    i = 0
    val blizzards = parse(input)

    //blizzards.foreach{println}
    var pos = Vec2(1, 0)
    val min = Vec2(1, 1)
    val max = Vec2(input.head.size, input.size)
    var target = Vec2(input.head.size - 2, input.size - 1)

    val r = shortest(blizzards, pos, target, max, 0)
    println((min, max, target, cache.size))
    //r._2.foreach(println)
    r._1
  }

  def shortest(
      blizzards: Seq[Blizzard],
      pos: Vec2,
      target: Vec2,
      maxPos: Vec2,
      minutes: Int
  ): (Int, Seq[Vec2]) = {

    assert(i < 1000000)
    i += 1

    val width = maxPos.x - 2
    val height = maxPos.y - 2
    val state = (pos, (minutes % width).toInt, (minutes % height).toInt)

    if (false) {
      printMap(blizzards, pos, target, maxPos)
      println()
      println(state)
      readLine()
    }


    if (best <= minutes || cache.contains(state) && cache(state) <= minutes) {
      (best, Seq.empty)
    } else if (pos == target) {
      println(s"reached target after $minutes min")
      best = Math.min(best, minutes)
      (best, Seq(pos))
    } else {
      cache = cache + (state -> minutes)

      var newPositions = neighbors(
        pos,
        min = Vec2(1, 1),
        max = maxPos,
        includeDiagonals = false
      ) :+ pos
      if (pos == Vec2(target.x, target.y - 1)) {
        newPositions = target +: newPositions
      }

      val newBlizzards = moveBlizzards(blizzards, maxPos)
      val blizzardPositions = newBlizzards.map(_.pos).toSet
      newPositions = newPositions.filterNot { p =>
        blizzardPositions.contains(p)
      }

      val results =
        newPositions.map(p =>
          shortest(newBlizzards, p, target, maxPos, minutes + 1)
        )

      val r = results.minByOption(_._1).getOrElse((large, Seq.empty))

      (r._1, pos +: r._2)
    }
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
}
