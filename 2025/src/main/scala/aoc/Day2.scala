package aoc

import aoc.Common.timed

object Day2:
    case class Range(from: Long, to: Long)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 2)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parseRanges(lines: List[String]): List[Range] =
        lines
            .mkString(",")
            .split(",")
            .filter(_.nonEmpty)
            .map:
                case s"$from-$to" => Range(from.toLong, to.toLong)
            .toList

    def isInvalidP1(id: Long): Boolean =
        val asString   = id.toString()
        val firstHalf  = asString.take(asString.length / 2)
        val secondHalf = asString.drop(asString.length / 2)
        firstHalf == secondHalf

    def isInvalidP2(id: Long): Boolean =
        val asString = id.toString()
        def loop(i: Int): Boolean =
            if i > asString.length() / 2 then false
            else
                val repeated = asString.take(i)
                var rest     = asString.drop(i)
                while rest.nonEmpty do
                    if rest.startsWith(repeated) then rest = rest.drop(repeated.length)
                    else return loop(i + 1)
                true
        loop(1)

    def part1(lines: List[String]): Long =
        val ranges = parseRanges(lines)
        ranges
            .map: range =>
                (range.from to range.to).filter(isInvalidP1).sum
            .sum

    def part2(lines: List[String]): Long =
        val ranges = parseRanges(lines)
        ranges
            .map: range =>
                (range.from to range.to).filter(isInvalidP2).sum
            .sum
