package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.Queue

object Day20 {

  case class Node(var n: Int, var prev: Node, var next: Node) {
    override def toString: String = n.toString
  }

  def part1(input: Seq[String]): Int = {

    var first: Node = null
    var prev: Node = null
    val nodes = input
      .map(_.toInt)
      .map { n =>
        val node = Node(n, prev = prev, next = null)
        if (first == null) {
          first = node
        }
        if (prev != null) {
          prev.next = node
        }

        prev = node
        node
      }
      .toArray
    first.prev = prev
    prev.next = first

    println("original:")
    printNodes(first)
    println()

    val queue = Queue[Node]()
    queue ++= nodes

    var i = 0
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      println(s"moving ${node.n} ${node.n} steps")

      move2(node, node.n, nodes.size)

      if (nodes.size < 20) {
        printNodes(first)
      }
      i += 1
    }
    printNodes(first)
    val zero = nodes.find(_.n == 0).get
    val a = find(zero, 1000, nodes.size)
    val b = find(a, 1000, nodes.size)
    val c = find(b, 1000, nodes.size)
    println(s"a: ${a.n}, b: ${b.n}, c: ${c.n}")

    a.n + b.n + c.n
  }

  private def move2(node: Node, steps: Int, size: Int) = {
    if (steps > 0) {
      var i = 0
      while (i < steps) {
        val next = node.next
        val secondNext = next.next

        node.prev.next = next
        next.prev = node.prev

        next.next = node
        node.prev = next
        node.next = secondNext
        secondNext.prev = node
        i += 1
      }
    } else if (steps < 0) {
      var i = 0
      while (i < -steps) {
        val prev = node.prev
        val secondPrev = prev.prev

        node.next.prev = prev
        prev.next = node.next

        prev.prev = node
        node.next = prev
        node.prev = secondPrev
        secondPrev.next = node
        i += 1
      }

    }

  }

  private def find(node: Node, steps: Int, size: Int) = {
    var next = node
    var i = 0
    if (steps > 0) {
      while (i < steps) {
        next = next.next
        i += 1
      }
    } else if (steps < 0) {
      next = next.prev
      while (i < -steps) {
        next = next.prev
        i += 1
      }
    }
    next
  }

  private def insertAfter(a: Node, b: Node): Unit = {
    if (a != b && a != b.next) {
      val aPrev = a.prev
      val aNext = a.next
      val bNext = b.next

      aPrev.next = aNext
      aNext.prev = aPrev

      b.next = a
      a.prev = b

      a.next = bNext
      bNext.prev = a
    }
  }

  def part2(input: Seq[String]): Int = {
    -1
  }

  def printNodes(start: Node) = {
    var node = start.next
    print(s"${start.n}, ")
    while (node.n != start.n) {
      print(node.n)
      print(", ")
      node = node.next
    }
    println()
    println()

  }
}
