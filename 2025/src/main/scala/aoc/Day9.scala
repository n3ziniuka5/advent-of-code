package aoc

import aoc.Common.timed
import aoc.Point
import math.abs

object Day9:
    case class Line(from: Point, to: Point):
        def fromX = from.x.min(to.x)
        def toX   = from.x.max(to.x)
        def fromY = from.y.min(to.y)
        def toY   = from.y.max(to.y)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 9)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parse(lines: List[String]): List[Point] =
        lines.map:
            case s"$x,$y" => Point(x.toLong, y.toLong)

    def squareAreas(points: List[Point]): List[(Point, Point, Long)] =
        points
            .combinations(2)
            .map: pair =>
                val p1   = pair.head
                val p2   = pair.tail.head
                val area = (abs(p1.x - p2.x) + 1) * (abs(p1.y - p2.y) + 1)
                (p1, p2, area)
            .toList

    def findWalls(points: List[Point]): List[Line] =
        val nonLooping = points
            .sliding(2)
            .map: pair =>
                val p1 = pair.head
                val p2 = pair.tail.head
                Line(p1, p2)
            .toList
        Line(points.head, points.last) +: nonLooping

    def inBounds(line: Line, walls: List[Line]): Boolean =
        def loop(remainingWalls: List[Line]): Boolean =
            remainingWalls match
                case head :: tail =>
                    val crossesHorizontal =
                        line.fromY >= head.fromY && line.toY <= head.toY && line.fromX < head.fromX && line.toX > head.toX
                    val crossesVertical =
                        line.fromX >= head.fromX && line.toX <= head.toX && line.fromY < head.fromY && line.toY > head.toY

                    if crossesHorizontal || crossesVertical then false else loop(tail)

                case Nil => true

        loop(walls)

    def part1(lines: List[String]): Long =
        val points = parse(lines)
        squareAreas(points).maxBy(_._3)._3

    def part2(lines: List[String]): Long =
        val points = parse(lines)
        val walls  = findWalls(points)

        squareAreas(points)
            .sortBy(_._3)(using Ordering[Long].reverse)
            .find: p =>
                val topLeft     = Point(p._1.x.min(p._2.x), p._1.y.min(p._2.y))
                val topRight    = Point(p._1.x.max(p._2.x), p._1.y.min(p._2.y))
                val bottomLeft  = Point(p._1.x.min(p._2.x), p._1.y.max(p._2.y))
                val bottomRight = Point(p._1.x.max(p._2.x), p._1.y.max(p._2.y))

                val top    = Line(topLeft, topRight)
                val bottom = Line(bottomLeft, bottomRight)
                val right  = Line(topRight, bottomRight)
                val left   = Line(topLeft, bottomLeft)

                val perimeter = List(top, bottom, right, left)

                perimeter.forall(inBounds(_, walls))
            .get
            ._3
