package aoc

import aoc.Common.timed
import zio.prelude.ZSet
import zio.prelude.newtypes.Natural

import scala.annotation.tailrec
import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day6.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def readInput(lines: List[String]): ZSet[Int, Long] = {
    ZSet.fromIterable(lines.head.split(',').map(_.toInt)).transform(Natural.unwrap(_).toLong)
  }

  @tailrec
  def solve(fish: ZSet[Int, Long], remainingDays: Int): Long = {
    if (remainingDays == 0) {
      fish.toMap.values.sum
    } else {
      val newFish = fish.flatMap {
        case 0 => ZSet(8, 6).transform(_.toLong)
        case i => ZSet(i - 1).transform(_.toLong)
      }
      solve(newFish, remainingDays - 1)
    }
  }

  def part1(lines: List[String]): Long = {
    solve(readInput(lines), 80)
  }

  def part2(lines: List[String]): Long = {
    solve(readInput(lines), 256)
  }
}
