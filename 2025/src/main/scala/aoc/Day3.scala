package aoc

import aoc.Common.timed

object Day3:
    case class Range(from: Long, to: Long)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 3)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def highestJoltage(line: String, length: Int): Long =
        val numbers = line.toVector.map(_.toString().toLong)

        def loop(numbers: List[Long], remaining: Int, result: List[Long]): Long =
            if remaining == 0 then result.reverse.mkString.toLong
            else
                val candidates   = numbers.dropRight(remaining - 1)
                val highestIndex = candidates.indices.maxBy(candidates)
                val newNumbers   = numbers.drop(highestIndex + 1)
                loop(newNumbers, remaining - 1, candidates(highestIndex) +: result)

        loop(numbers.toList, length, Nil)

    def part1(lines: List[String]): Long =
        lines.map(highestJoltage(_, 2)).sum

    def part2(lines: List[String]): Long =
        lines.map(highestJoltage(_, 12)).sum
