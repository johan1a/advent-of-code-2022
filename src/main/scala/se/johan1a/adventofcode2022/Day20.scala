package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.Queue

object Day20 {

  case class Node(var n: Int, var prev: Node, var next: Node) {
//    override def toString: String = s"[${prev.n.toString}, ${n.toString}, ${next.n.toString}]"
    override def toString: String = n.toString
  }

  def part1(input: Seq[String]): Int = {

    var first: Node = null
    var prev: Node = null
    val nodes = input.map(_.toInt).map { n =>
      val node = Node(n, prev = prev, next = null)
      if (first == null) {
        first = node
      }
      if (prev != null) {
        prev.next = node
      }

      prev = node
      node
    }.toArray
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
      assert(node.n == nodes(i).n)
      println(s"moving ${node.n} ${node.n} steps")
      val next = find(node, node.n, nodes.size)
      println(s"$node -> $next")
      insertAfter(node, next)
      if(nodes.size < 20) {
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

  private def find(node: Node, steps: Int, size: Int) = {
    var next = node
    var i = 0
    if (steps >= 0) {
      while (i < steps) {
        next = next.next
        i += 1
      }
    } else {
      while (i < -steps + 1) {
        next = next.prev
        i += 1
      }
    }
    next
  }

  private def insertAfter(a: Node, b: Node): Unit = {
    if (a != b) {

      val aPrev = a.prev
      val aNext = a.next
      val bPrev = b.prev
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

  }
}
