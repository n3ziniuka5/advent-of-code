package aoc

import aoc.Common.timed

import scala.io.Source

object Day8 {
  case class Line(input: List[Set[Char]], output: List[Set[Char]])

  val segmentMapping = Map(
    Set('a', 'b', 'c', 'e', 'f', 'g')      -> 0,
    Set('c', 'f')                          -> 1,
    Set('a', 'c', 'd', 'e', 'g')           -> 2,
    Set('a', 'c', 'd', 'f', 'g')           -> 3,
    Set('b', 'c', 'd', 'f')                -> 4,
    Set('a', 'b', 'd', 'f', 'g')           -> 5,
    Set('a', 'b', 'd', 'e', 'f', 'g')      -> 6,
    Set('a', 'c', 'f')                     -> 7,
    Set('a', 'b', 'c', 'd', 'e', 'f', 'g') -> 8,
    Set('a', 'b', 'c', 'd', 'f', 'g')      -> 9
  )

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day8.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def constructLine(str: String): Line = {
    val Array(input, output) = str
      .split('|')
      .map(_.trim)
      .map(_.split(' ').map(_.toCharArray.toSet).toList)

    Line(input, output)
  }

  def part1(lines: List[String]): Int = {
    lines
      .map(constructLine)
      .map { line =>
        line.output.count { digit =>
          digit.size == 2 || digit.size == 3 || digit.size == 4 || digit.size == 7
        }
      }
      .sum
  }

  def decodeInput(input: List[Set[Char]]): Map[Char, Char] = {
    val initialPossibilityMap = ('a' to 'g').map(_ -> ('a' to 'g').toSet).toMap

    val finalPossibilities = input.sortBy(_.size).foldLeft(initialPossibilityMap) { (acc, in) =>
      in.size match {
        case 2 =>
          in.foldLeft(acc.view.mapValues(s => s - 'c' - 'f').toMap) { (acc, c) =>
            acc + (c -> (acc(c) ++ Set('c', 'f')))
          }
        case 3 =>
          in.foldLeft(acc) { (acc, c) =>
            acc + (c -> acc(c).intersect(Set('a', 'c', 'f')))
          }.view
            .mapValues(v => if (v.size != 1) v.filter(_ != 'a') else v)
            .toMap
        case 4 =>
          in.foldLeft(acc) { (acc, c) =>
            acc + (c -> acc(c).intersect(Set('b', 'c', 'd', 'f')))
          }
        case 5 =>
          if (in.count(acc(_) == Set('c', 'f')) == 2) {
            in.foldLeft(acc) { (acc, c) =>
              acc + (c -> (acc(c) - 'b' - 'e'))
            }.view
              .mapValues(v => if (v.contains('e')) Set('e') else v)
              .mapValues(v => if (v.size != 1) v.filter(_ != 'd') else v)
              .toMap
          } else {
            acc
          }
        case 6 =>
          if (in.count(acc(_) == Set('c', 'f')) == 1) {
            val c = in.find(acc(_) == Set('c', 'f')).get
            (acc + (c -> Set('f'))).view.mapValues(v => if (v.size != 1) v.filter(_ != 'f') else v).toMap
          } else {
            acc
          }

        case _ => acc
      }
    }

    finalPossibilities.view.mapValues(_.head).toMap
  }

  def part2(lines: List[String]): Int = {
    lines
      .map(constructLine)
      .map { line =>
        val decoded = decodeInput(line.input)

        val digits = line.output
          .map(_.flatMap(decoded.get))
          .flatMap(segmentMapping.get)

        digits(0) * 1000 + digits(1) * 100 + digits(2) * 10 + digits(3)
      }
      .sum
  }
}
