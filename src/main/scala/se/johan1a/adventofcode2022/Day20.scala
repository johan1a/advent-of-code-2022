package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.Queue

object Day20 {

  case class Node(var n: Long, var prev: Node, var next: Node) {
    override def toString: String = n.toString
  }

  def part1(input: Seq[String]): Long = {
    val nodes = parse(input)
    mix(nodes)
    coordinateSum(nodes)
  }

  def part2(input: Seq[String]): Long = {
    val decryptionKey = 811589153L
    val nodes = parse(input, decryptionKey)
    0.until(10).foreach { _ =>
      mix(nodes)
    }
    coordinateSum(nodes)
  }

  private def mix(nodes: Array[Node]): Unit = {
    var i = 0
    while (i < nodes.size) {
      val node = nodes(i)
      move(node, node.n % (nodes.size - 1))
      i += 1
    }
  }

  private def coordinateSum(nodes: Array[Node]) = {
    val zero = nodes.find(_.n == 0).get
    val a = find(zero, 1000)
    val b = find(a, 1000)
    val c = find(b, 1000)

    a.n + b.n + c.n
  }


  private def move(node: Node, steps: Long) = {
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

  private def find(node: Node, steps: Long) = {
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

  private def parse(input: Seq[String], factor: Long = 1) = {
    var first: Node = null
    var prev: Node = null
    val nodes = input
      .map(_.toInt)
      .map { n =>
        val node = Node(n * factor, prev = prev, next = null)
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
    nodes
  }

}
