package se.johan1a.adventofcode2022

object Utils {

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
