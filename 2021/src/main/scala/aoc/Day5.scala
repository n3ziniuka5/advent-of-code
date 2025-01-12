package aoc

import aoc.Common.timed
import zio.prelude.MultiSet

import scala.annotation.tailrec
import scala.io.Source

object Day5:
    case class Point(x: Int, y: Int)
    case class Line(from: Point, to: Point)

    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day5.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parseInput(lines: List[String]): List[Line] =
        lines.map { lineString =>
            val Array(from, to) = lineString.split("->").map(_.trim).map { pointString =>
                val Array(x, y) = pointString.split(',').map(_.toInt)
                Point(x, y)
            }
            Line(from, to)
        }

    def isStraightLine(line: Line): Boolean =
        line.from.x == line.to.x || line.from.y == line.to.y

    def pointsForLine(line: Line, cond: (Int, Int) => Boolean): IndexedSeq[Point] =
        for
            x <- Math.min(line.from.x, line.to.x) to Math.max(line.from.x, line.to.x)
            y <- Math.min(line.from.y, line.to.y) to Math.max(line.from.y, line.to.y)
            if cond(x, y)
        yield Point(x, y)

    @tailrec
    def overlaps(lines: List[Line], pointCounts: MultiSet[Point] = MultiSet.empty): Int =
        lines match
            case head :: tail =>
                val points =
                    if isStraightLine(head) then pointsForLine(head, (_, _) => true)
                    else pointsForLine(head, (x, y) => Math.abs(head.from.x - x) == Math.abs(head.from.y - y))

                overlaps(tail, pointCounts <> MultiSet.fromIterable(points))
            case Nil => pointCounts.toMap.count(_._2 > 1)

    def part1(lines: List[String]): Int =
        val input         = parseInput(lines)
        val straightLines = input.filter(isStraightLine)
        overlaps(straightLines)

    def part2(lines: List[String]): Int =
        overlaps(parseInput(lines))
