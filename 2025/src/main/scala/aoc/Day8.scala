package aoc

import aoc.Common.timed
import math.pow

object Day8:
    case class Point(x: Long, y: Long, z: Long)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 8)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parse(lines: List[String]): List[Point] =
        lines.map:
            case s"$x,$y,$z" => Point(x.toLong, y.toLong, z.toLong)

    var distanceCache: Option[Vector[(Point, Point, Double)]] = None
    def makeConnections(points: List[Point], limit: Int): (Map[Point, Set[Point]], (Point, Point)) =
        val distances = distanceCache.getOrElse:
            points
                .combinations(2)
                .map: pair =>
                    val p1 = pair.head
                    val p2 = pair.tail.head
                    val distanceSquared =
                        pow((p1.x - p2.x).toDouble, 2) + pow((p1.y - p2.y).toDouble, 2) + pow((p1.z - p2.z).toDouble, 2)

                    (p1, p2, distanceSquared)
                .toVector
                .sortBy(_._3)
        distanceCache = Some(distances)

        val shortestConnections = distances.take(limit)
        val lastConnection      = shortestConnections.last

        val connections = shortestConnections.foldLeft(Map.empty[Point, Set[Point]].withDefaultValue(Set.empty)):
            (result, connection) =>
                val (p1, p2, _) = connection
                result + (p1 -> (result(p1) + p2)) + (p2 -> (result(p2) + p1))

        (connections, (lastConnection._1, lastConnection._2))

    def search(from: Point, connections: Map[Point, Set[Point]]): Set[Point] =
        def loop(remaining: List[Point], visited: Set[Point]): Set[Point] =
            remaining match
                case head :: tail =>
                    if visited.contains(head) then loop(tail, visited)
                    else
                        val newPoints = connections(head)
                        loop(newPoints.toList ++ tail, visited + head)
                case Nil =>
                    visited
        loop(List(from), Set.empty)

    def findCircuits(connections: Map[Point, Set[Point]]): List[Set[Point]] =
        def loop(remaining: Set[Point], visited: Set[Point], result: List[Set[Point]]): List[Set[Point]] =
            if remaining.isEmpty then result
            else
                val searchFrom = remaining.head
                if visited.contains(searchFrom) then loop(remaining - searchFrom, visited, result)
                else
                    val circuit = search(searchFrom, connections)
                    loop(remaining - searchFrom, visited ++ circuit, circuit +: result)

        loop(connections.keySet, Set.empty, Nil)

    def part1(lines: List[String]): Long =
        val points           = parse(lines)
        val limit            = if lines.size == 20 then 10 else 1000
        val (connections, _) = makeConnections(points, limit)
        val circuits         = findCircuits(connections)
        circuits.map(_.size).sorted(using Ordering[Int].reverse).take(3).product

    def part2(lines: List[String]): Long =
        val points    = parse(lines)
        val numPoints = points.size

        def binSearch(minBoundary: Int, maxBoundary: Int): Long =
            val middle                        = (minBoundary + maxBoundary) / 2
            val (connections, lastConnection) = makeConnections(points, middle)
            val circuits                      = findCircuits(connections)

            if circuits.sizeIs > 1 || circuits.head.size != numPoints then binSearch(middle + 1, maxBoundary)
            else if minBoundary != maxBoundary then binSearch(minBoundary, middle)
            else lastConnection._1.x * lastConnection._2.x

        binSearch(0, numPoints * (numPoints - 1) / 2)
