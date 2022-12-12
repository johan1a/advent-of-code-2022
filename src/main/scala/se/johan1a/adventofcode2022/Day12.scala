package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue


object Day12 {

  def part1(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    var start = Vec2(-1, -1)
    var end = Vec2(-1, -1)
    grid.indices.foreach { row =>
      grid(row).indices.foreach { col =>
        val char = grid(row)(col)
        if(char == 'S') {
          start = Vec2(col, row)
          grid(row)(col) = 'a'
        } else if (char == 'E') {
          end = Vec2(col, row)
          grid(row)(col) = 'z'
        }
      }
    }
    grid.foreach(println)
    println(start)
    println(end)
    var currHeight = 'a'
    var endHeight = 'z'

    val queue = PriorityQueue[(Int, Vec2)]()(Ordering.by(_._1))
    grid.indices.foreach { row =>
      grid(row).indices.foreach { col =>
        queue += ((Int.MaxValue, Vec2(col, row)))
      }
    }


    val dists = Map[Vec2, Int](start -> 0)
    var prev = Map[Vec2, Vec2]()

    var pos = start
    while (queue.nonEmpty) {
      val u = queue.dequeue()._2

      val uNeighbors = neighbors(u,
        min = Vec2(0, 0),
        max = getMax(grid),
        includeDiagonals=false).filter(n => heightDiffOk(grid, u, n))
      uNeighbors.map(neighbor => {
        val dist = dists.get(u).map(_ + 1).getOrElse(Int.MaxValue)
        if (dist < dists.get(neighbor).getOrElse(Int.MaxValue)) {
          dists.addOne(neighbor, dist)
          queue += ((dist, neighbor))
        }
      })
    }

    dists(end)
  }

  private def heightDiffOk(grid: ArrayBuffer[ArrayBuffer[Char]], a: Vec2, b: Vec2) = {
    val aHeight = grid(a.y.toInt)(a.x.toInt).toInt
    val bHeight = grid(b.y.toInt)(b.x.toInt).toInt
    val res = bHeight <= aHeight || aHeight == bHeight - 1

    //println(s"height ok for $a->$b: $res")
    res
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
