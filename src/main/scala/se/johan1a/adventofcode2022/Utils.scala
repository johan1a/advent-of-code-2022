package se.johan1a.adventofcode2022

object Utils {

  case class Vec2(x: Long, y: Long)
  case class Vec3(x: Long, y: Long, z: Long)

  def add(a: Vec2, b: Vec2): Vec2 = Vec2(a.x + b.x, a.y + b.y)
  def sub(a: Vec2, b: Vec2): Vec2 = Vec2(a.x - b.x, a.y - b.y)

  def add(a: Vec3, b: Vec3): Vec3 = Vec3(a.x + b.x, a.y + b.y, a.z + b.z)
  def sub(a: Vec3, b: Vec3): Vec3 = Vec3(a.x - b.x, a.y - b.y, a.z - b.z)

  def split(
      input: Seq[String],
      isEmpty: String => Boolean = _.isBlank
  ): Seq[Seq[String]] = {
    var groups = Seq[Seq[String]]()
    var i = 0
    while (i < input.size) {
      var group = Seq[String]()
      while (i < input.size && !isEmpty(input(i))) {
        group = group :+ input(i)
        i += 1
      }
      if (group.nonEmpty) {
        groups = groups :+ group
      }
      while (i < input.size && isEmpty(input(i))) {
        i += 1
      }
    }
    groups
  }

}
