package aoc

import aoc.Common.timed
import zio.prelude.MultiSet
import zio.prelude.ZSet

import scala.annotation.tailrec
import scala.io.Source
import zio.prelude.newtypes.Natural

object Day14 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day14.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  case class Input(start: String, polymers: Map[String, Char])

  def readInput(lines: List[String]): Input = {
    val start = lines.head

    val polymers = lines
      .drop(2)
      .map { transformation =>
        val Array(from, to) = transformation.split("->").map(_.trim)
        from -> to.head
      }
      .toMap

    Input(start, polymers)
  }

  @tailrec
  def applySteps(
    pairs: ZSet[String, Long],
    charCount: ZSet[Char, Long],
    remaining: Int,
    polymers: Map[String, Char]
  ): Long = {
    if (remaining == 0) {
      val charCountMap = charCount.toMap.toVector.sortBy(_._2)
      charCountMap.last._2 - charCountMap.head._2
    } else {
      val (newPairs, newCharCount) = pairs.toMap.foldLeft((ZSet.empty: ZSet[String, Long], charCount)) {
        case ((accPair, accChars), (pair, pairCount)) =>
          val charToInsert = polymers(pair)
          val newPairs = ZSet.fromMap(
            Map(
              s"${pair.head}$charToInsert"      -> pairCount,
              s"$charToInsert${pair.charAt(1)}" -> pairCount
            )
          )

          (accPair <> newPairs, accChars <> ZSet.fromMap(Map(charToInsert -> pairCount)))
      }

      applySteps(newPairs, newCharCount, remaining - 1, polymers)
    }
  }

  def solve(input: Input, steps: Int): Long = {
    applySteps(
      MultiSet.fromIterable(input.start.sliding(2).toList).transform(Natural.unwrap(_).toLong),
      MultiSet.fromIterable(input.start).transform(Natural.unwrap(_).toLong),
      steps,
      input.polymers
    )
  }

  def part1(lines: List[String]): Long = {
    solve(readInput(lines), 10)
  }

  def part2(lines: List[String]): Long = {
    solve(readInput(lines), 40)
  }
}
