package aoc

import aoc.Common.timed

object Day5:
    case class Range(from: Long, to: Long):
        def size: Long = to - from + 1

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 5)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parse(lines: List[String]): (List[Range], List[Long]) =
        val ranges = lines
            .takeWhile(_.nonEmpty)
            .map:
                case s"$from-$to" => Range(from.toLong, to.toLong)
        val numbers = lines.drop(ranges.size + 1).map(_.toLong)
        (ranges, numbers)

    def part1(lines: List[String]): Long =
        val (ranges, numbers) = parse(lines)
        val result = numbers.count: num =>
            ranges.exists: range =>
                num >= range.from && num <= range.to
        result

    def part2(lines: List[String]): Long =
        val (ranges, _)  = parse(lines)
        val sortedRanges = ranges.sortBy(r => (r.from, r.to))

        def loop(placedRanges: List[Range], toPlace: List[Range]): Long =
            toPlace match
                case head :: tail =>
                    val maybeLastPlaced = placedRanges.headOption
                    maybeLastPlaced match
                        case None => loop(head +: placedRanges, tail)
                        case Some(lastPlaced) =>
                            if head.from <= lastPlaced.to then
                                loop(
                                  Range(lastPlaced.from, Math.max(lastPlaced.to, head.to)) +: placedRanges.tail,
                                  tail
                                )
                            else loop(head +: placedRanges, tail)
                case Nil => placedRanges.map(_.size).sum

        loop(Nil, sortedRanges)
