package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.Queue

object Day18 {

  def part1(input: Seq[String]): Int = {
    val cubes = parse(input)
    var sum = 0
    cubes
      .foreach {
        case (cube: Vec3) => {
          val nn = neighbors3(cube)
          assert(nn.size == 6)
          val r = nn
            .map(neighbor => {
              if (!cubes.contains(neighbor)) {
                1
              } else {
                0
              }
            })
            .sum
          sum += r
        }
      }

    sum
  }

  val maxPocketSize = 1100

  def part2(input: Seq[String]): Int = {
    val cubes = parse(input)
    var pockets = Set[Vec3]()
    var air = Set[Vec3]()
    var sum = 0
    var i = 0
    cubes
      .foreach {
        case (cube: Vec3) => {
          i += 1
          println(s"checking cube $i of ${cubes.size} air size: ${air.size}, pocket size: ${pockets.size}")
          val nn = neighbors3(cube)
          assert(nn.size == 6)
          val r = nn
            .map(neighbor => {
              if (cubes.contains(neighbor)) {
                // println(s"not counting $neighbor - in cubes")
                0
              } else if (pockets.contains(neighbor)) {
                // println(s"not counting $neighbor - in pocket")
                0
              } else if (air.contains(neighbor)) {
                1
              } else {
                // println(s"flood at $neighbor")
                val pocket = flood(cubes, air, neighbor)
                pocket match {
                  case Pocket(cc) =>
                    pockets = pockets ++ cc
                    0
                  case Air(cc) =>
                    air = air ++ cc
                    1
                }
              }
            })
            .sum
          sum += r
        }
      }

    sum
  }

  sealed trait PocketT
  case class Pocket(cubes: Set[Vec3]) extends PocketT
  case class Air(cubes: Set[Vec3]) extends PocketT

  def flood(cubes: Set[Vec3], air: Set[Vec3], pos: Vec3): PocketT = {
    var pocket = Set[Vec3]()
    val queue = Queue[Vec3](pos)

    while (queue.nonEmpty && pocket.size < maxPocketSize) {
      if(pocket.size %  100 == 0){
        println(s"queue size: ${queue.size}, pocket size: ${pocket.size}")
      }
      var current = queue.dequeue()

      if (!cubes.contains(current)) {
        pocket = pocket + current
      }
      if (air.contains(current)) {
        return Air(pocket)
      }
      queue.enqueueAll(neighbors3(current).filterNot(c => {
        pocket.contains(c) || cubes.contains(c)
      }))
    }

    if (pocket.size >= maxPocketSize) {
      Air(pocket)
    } else {
      Pocket(pocket)
    }
  }

  def parse(input: Seq[String]): Set[Vec3] = {
    input
      .map(line => {
        val nn = numbers(line)
        Vec3(nn(0), nn(1), nn(2))
      })
      .toSet
  }
}
