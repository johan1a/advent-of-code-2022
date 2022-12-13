package se.johan1a.adventofcode2022

import Utils._
import scala.collection.mutable.{Stack, Buffer}

object Day13 {
  sealed trait Packet
  case class PacketList(packets: Buffer[Packet]) extends Packet {
    override def toString = s"[${packets.map(_.toString).mkString(",")}]"
  }
  case class Num(n: Int) extends Packet {
    override def toString = n.toString
  }

  sealed trait Result
  case class OK() extends Result
  case class Continue() extends Result
  case class Fail() extends Result

  def part1(input: Seq[String]): Int = {
    val pairs = split(input)
    println(pairs)
    val indices = pairs.zipWithIndex.map { case (pair: Seq[String], index: Int) =>
      val result = check(parsePacket(pair.head), parsePacket(pair.last))
      println(s"\na: ${pair.head}, b: ${pair.last}, result: ${result}")
      if (result == OK() || result == Continue()) {
        println(s"valid: true")
        index + 1
      } else {
        println(s"valid: false")
        0
      }
    }.filter(_ != 0)
    println(s"Valid indices: ${indices}")

    indices.sum
  }



  private def check(a: Packet, b: Packet): Result = {
    println(s"\na: $a")
    println(s"b: $b")

    (a, b) match {
      case (PacketList(aa), PacketList(bb)) =>
        var j = 0
        var result: Result = Continue()
        println(s"j=$j")
        while (j < aa.size && j < bb.size && result == Continue()) {
            result = check(aa(j), bb(j))
            println(s"result: $result, j=$j")
            j += 1
        }
        val r = if (result == Continue()) {
          if (aa.size < bb.size) {
            OK()
          } else if (aa.size > bb.size) {
            Fail()
          } else {
            Continue()
          }
        } else {
          result
        }

        println(s"result: $r")
        r

      case (Num(aNum), PacketList(bb))      => check(PacketList(Buffer(a)), b)
      case (PacketList(aa), Num(bNum))      => check(a, PacketList(Buffer(b)))
      case (Num(aNum), Num(bNum))           => if(aNum < bNum) {
        OK()
      } else if(aNum == bNum) {
        Continue()
      } else {
        Fail()
      }
    }
  }

  private def parsePacket(input: String): Packet = {
    val stack = Stack[PacketList]()
    var i = 1 // assume it starts and ends with [
    var current = PacketList(Buffer())
    while (i < input.size - 1) {
      val next = input.charAt(i)
      next match {
        case ',' =>
          i += 1
        case '[' =>
          stack.push(current)
          current = PacketList(Buffer())
          i += 1
        case ']' => // todo fails on last ]
          val parent = stack.pop()
          parent.packets += current
          current = parent
          i += 1
        case digit =>
          var j = i + 1
          var digits = Seq(digit)
          while (j < input.size && input.charAt(j).isDigit) {
            digits = digits :+ input.charAt(j)
            j += 1
          }
          val num = digits.mkString.toInt
          current.packets += Num(num)
          i += (j - i)
      }
    }
    current
  }

  def part2(input: Seq[String]): Int = {
    -1
  }
}
