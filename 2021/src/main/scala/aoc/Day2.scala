package aoc

import aoc.Common.timed
import scala.io.Source

object Day2 {
  case class Position1(horizontal: Int, depth: Int)
  case class Position2(horizontal: Int, aim: Int, depth: Int)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day2.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def part1(lines: List[String]): Int = {
    val finalPosition = lines.foldLeft(Position1(0, 0)) { (accum, move) =>
      val Array(direction, units) = move.split(" ")
      direction match {
        case "forward" => accum.copy(horizontal = accum.horizontal + units.toInt)
        case "down"    => accum.copy(depth = accum.depth + units.toInt)
        case "up"      => accum.copy(depth = accum.depth - units.toInt)
      }
    }

    finalPosition.depth * finalPosition.horizontal
  }

  def part2(lines: List[String]): Int = {
    val finalPosition = lines.foldLeft(Position2(0, 0, 0)) { (accum, move) =>
      val Array(direction, units) = move.split(" ")
      direction match {
        case "forward" =>
          accum.copy(horizontal = accum.horizontal + units.toInt, depth = accum.depth + accum.aim * units.toInt)
        case "down" => accum.copy(aim = accum.aim + units.toInt)
        case "up"   => accum.copy(aim = accum.aim - units.toInt)
      }
    }

    finalPosition.depth * finalPosition.horizontal
  }
}
