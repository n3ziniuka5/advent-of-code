package aoc

import aoc.Common.timed

object Day4:
    case class Range(from: Long, to: Long)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 4)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def canAccess(map: Map2d[Char]): Iterable[Point] =
        map.underlying
            .filter: (p, c) =>
                c match
                    case '.' => false
                    case '@' =>
                        p.adjacentDiagonal.count(pp => map.get(pp).contains('@')) < 4
            .keys

    def part1(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)
        canAccess(map).size

    def part2(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)

        def loop(map: Map2d[Char], result: Long): Long =
            val toRemove = canAccess(map)
            if toRemove.isEmpty then result
            else
                val newMap = Map2d(map.underlying -- toRemove)
                loop(newMap, result + toRemove.size)

        loop(map, 0L)
