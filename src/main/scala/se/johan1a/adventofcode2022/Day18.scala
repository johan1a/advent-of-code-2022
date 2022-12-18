package se.johan1a.adventofcode2022

import Utils._

object Day18 {

  def part1(input: Seq[String]): Int = {
    val cubes = parse(input)
    var sum = 0
    val r1 = cubes
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

  def part2(input: Seq[String]): Int = {
    -1
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
