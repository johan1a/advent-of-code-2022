package se.johan1a.adventofcode2022

import scala.util.{Try, Success}

object Day21 {

  sealed trait Op
  case class Eq(a: String, b: String) extends Op
  case class Add(a: String, b: String) extends Op
  case class Sub(a: String, b: String) extends Op
  case class Mul(a: String, b: String) extends Op
  case class Div(a: String, b: String) extends Op
  case class Num(value: Long) extends Op
  case class Var(id: String) extends Op

  sealed trait Expr
  case class Number(n: Long) extends Expr
  case class VarExpr(id: String) extends Expr
  case class EqExpr(a: Expr, b: Expr) extends Expr
  case class AddExpr(a: Expr, b: Expr) extends Expr
  case class MulExpr(a: Expr, b: Expr) extends Expr
  case class SubExpr(a: Expr, b: Expr) extends Expr
  case class DivExpr(a: Expr, b: Expr) extends Expr

  def part1(input: Seq[String]): Long = {
    val instructions = input.map(parse).toMap
    eval(instructions, "root")
  }

  private def eval(instructions: Map[String, Op], id: String): Long = {
    instructions(id) match {
      case Num(value) => value
      case Add(a, b)  => eval(instructions, a) + eval(instructions, b)
      case Sub(a, b)  => eval(instructions, a) - eval(instructions, b)
      case Mul(a, b)  => eval(instructions, a) * eval(instructions, b)
      case Div(a, b)  => eval(instructions, a) / eval(instructions, b)
    }
  }

  private def parse(line: String): (String, Op) = {
    line match {
      case s"$id: $a + $b" => id -> Add(a, b)
      case s"$id: $a - $b" => id -> Sub(a, b)
      case s"$id: $a * $b" => id -> Mul(a, b)
      case s"$id: $a / $b" => id -> Div(a, b)
      case s"$id: $n"      => id -> Num(n.toLong)
    }
  }

  def part2(input: Seq[String]): Long = {
    var instructions = input.map(parse2).toMap
    println(instructions)
    val tree = buildTree(instructions, "root").asInstanceOf[EqExpr]
    println(tree.a)
    println(tree.b)
    (Try(eval2(tree.a)), Try(eval2(tree.b))) match {
      case (Success(a), _) => calculate(a, tree.b)
      case (_, Success(b)) => calculate(b, tree.a)
    }
  }

  private def calculate(value: Long, expr: Expr): Long = {
    expr match {
      case VarExpr(id) => value
      case AddExpr(a, b) =>
        val aTry = Try(eval2(a))
        val bTry = Try(eval2(b))
        (aTry, bTry) match {
          case (Success(n), _) => calculate(value - n, b)
          case (_, Success(n)) => calculate(value - n, a)
        }
      case SubExpr(a, b) =>
        (Try(eval2(a)), Try(eval2(b))) match {
          case (Success(n), _) => calculate(-(value - n), b)
          case (_, Success(n)) => calculate(value + n, a)
        }
      case MulExpr(a, b) =>
        (Try(eval2(a)), Try(eval2(b))) match {
          case (Success(n), _) => calculate(value / n, b)
          case (_, Success(n)) => calculate(value / n, a)
        }
      case DivExpr(a, b) =>
        (Try(eval2(a)), Try(eval2(b))) match {
          case (Success(n), _) => calculate(n / value, b)
          case (_, Success(n)) => calculate(value * n, a)
        }
    }
  }

  private def buildTree(instructions: Map[String, Op], id: String): Expr = {
    instructions(id) match {
      case Num(value) => Number(value)
      case Var(id) =>
        VarExpr(id)
      case Eq(a, b) =>
        EqExpr(buildTree(instructions, a), buildTree(instructions, b))
      case Add(a, b) =>
        AddExpr(buildTree(instructions, a), buildTree(instructions, b))
      case Sub(a, b) =>
        SubExpr(buildTree(instructions, a), buildTree(instructions, b))
      case Mul(a, b) =>
        MulExpr(buildTree(instructions, a), buildTree(instructions, b))
      case Div(a, b) =>
        DivExpr(buildTree(instructions, a), buildTree(instructions, b))
    }
  }

  private def eval2(expr: Expr): Long = {
    expr match {
      case Number(value) => value
      case AddExpr(a, b) => eval2(a) + eval2(b)
      case SubExpr(a, b) => eval2(a) - eval2(b)
      case MulExpr(a, b) => eval2(a) * eval2(b)
      case DivExpr(a, b) => eval2(a) / eval2(b)
    }
  }

  private def parse2(line: String): (String, Op) = {
    line match {
      case s"root: $a $x $b" => "root" -> Eq(a, b)
      case s"humn: $n"       => "humn" -> Var("humn")
      case s"$id: $a + $b"   => id -> Add(a, b)
      case s"$id: $a - $b"   => id -> Sub(a, b)
      case s"$id: $a * $b"   => id -> Mul(a, b)
      case s"$id: $a / $b"   => id -> Div(a, b)
      case s"$id: $n"        => id -> Num(n.toLong)
    }
  }
}
