package se.johan1a.adventofcode2022

import scala.collection.mutable.Buffer

object Day07 {

  case class File(name: String, size: Long)
  case class Dir(
      name: String,
      absoluteName: String,
      dirs: Buffer[Dir],
      files: Buffer[File],
      var parent: Dir
  )

  def part1(originalInput: Seq[String]): Long = {
    val sizes = getSizes(originalInput)
    sizes.filter(_._2 <= 100000).map(_._2).sum
  }

  def part2(input: Seq[String]): Long = {
    val totalSpace = 70000000L
    val unusedSpaceNeeded = 30000000L

    val sizes = getSizes(input)
    val usedSpace = sizes.find(_._1 == "/").map(_._2).get
    val unusedSpace = totalSpace - usedSpace
    val spaceToFreeUp = unusedSpaceNeeded - unusedSpace

    sizes.sortBy(_._2).find(_._2 > spaceToFreeUp).map(_._2).get
  }

  def getSizes(originalInput: Seq[String]): Seq[(String, Long, String)] = {
    val input = scala.collection.mutable.ArrayBuffer[String]()
    input.addAll(originalInput)
    var root = Dir("/", "/", Buffer.empty, Buffer.empty, null)
    var currentDir = root

    input.foreach { line =>
      line match {
        case cmd if cmd.startsWith("$ cd ..") =>
          currentDir = currentDir.parent
        case cmd if cmd.startsWith("$ cd /") =>
          currentDir = root
        case cmd if cmd.startsWith("$ cd ") =>
          val split = cmd.split(" cd ")
          val destName = split.last.trim
          val dest = currentDir.dirs.find(_.name == destName).get
          currentDir = dest
        case cmd if cmd.startsWith("$ ls") =>
        // No-op
        case dir if dir.startsWith("dir ") =>
          val name = dir.split("dir ").last.trim
          val absoluteName = if (currentDir.name == "/") {
            "/" + name
          } else {
            currentDir.parent.absoluteName + "/" + name
          }
          val newDir =
            Dir(name, absoluteName, Buffer.empty, Buffer.empty, currentDir)
          currentDir.dirs.addOne(newDir)
        case file =>
          val split = file.split(" ")
          val size = split.head.toLong
          val name = split.last
          currentDir.files.addOne(File(name, size))
      }
    }

    findSize(root)
  }

  private def findSize(dir: Dir): Seq[(String, Long, String)] = {
    val filesSize = dir.files.map(_.size).sum
    val children = dir.dirs.flatMap(findSize)

    val childrenTotalSize = children
      .filter(c => dir.dirs.map(_.absoluteName).contains(c._3))
      .map(c => c._2)
      .sum
    Seq((dir.name, filesSize + childrenTotalSize, dir.absoluteName)) ++ children
  }

  private def printTree(node: Dir, indent: String = ""): Unit = {
    println(s"- $indent${node.name} (dir)")
    node.dirs.map(dir => {
      printTree(dir, indent + "  ")
    })
    node.files.map(file => {
      println(s"- $indent  ${file.name} (file, size=${file.size})")
    })
  }
}
