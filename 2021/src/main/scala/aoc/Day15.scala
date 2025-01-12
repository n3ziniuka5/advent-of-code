package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day15 {
  case class Point(x: Int, y: Int) {
    def neighbours: Set[Point] =
      Set(Point(x + 1, y), Point(x - 1, y), Point(x, y + 1), Point(x, y - 1))
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day15.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def readPoints(lines: List[String]): Map[Point, Int] = {
    lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (char, x) =>
        Point(x, y) -> char.toString.toInt
      }
    }.toMap
  }

  case class PointWithDistance(point: Point, distance: Int)

  @tailrec
  def shortestPath(
    toVisit: mutable.PriorityQueue[PointWithDistance],
    points: Map[Point, Int],
    maxCoord: Int,
    visited: Set[Point] = Set.empty
  ): Int = {
    val head = toVisit.dequeue()
    if (head.point.x == maxCoord && head.point.y == maxCoord) {
      head.distance
    } else if (visited.contains(head.point)) {
      shortestPath(toVisit, points, maxCoord, visited)
    } else {
      head.point.neighbours
        .filter(p => p.x >= 0 && p.y >= 0)
        .filter(p => p.x <= maxCoord && p.y <= maxCoord)
        .foreach { p =>
          toVisit.enqueue(PointWithDistance(p, head.distance + points(p)))
        }
      shortestPath(toVisit, points, maxCoord, visited + head.point)
    }
  }

  def multiplyInput(input: Map[Point, Int]): Map[Point, Int] = {
    val size = Math.sqrt(input.size).toInt

    input.flatMap { case (p, energy) =>
      for {
        yAdd <- 0 to 4
        xAdd <- 0 to 4
      } yield {
        val newEnergy      = energy + xAdd + yAdd
        val finalNewEnergy = if (newEnergy >= 10) newEnergy - 9 else newEnergy
        p.copy(
          x = p.x + xAdd * size,
          y = p.y + yAdd * size
        ) -> finalNewEnergy
      }
    }
  }

  def part1(lines: List[String]): Int = {
    val input = readPoints(lines)

    shortestPath(
      mutable.PriorityQueue[PointWithDistance](PointWithDistance(Point(0, 0), 0))(
        Ordering.by[PointWithDistance, Int](_.distance).reverse
      ),
      input,
      Math.sqrt(input.size).toInt - 1
    )
  }

  def part2(lines: List[String]): Int = {
    val input = multiplyInput(readPoints(lines))

    shortestPath(
      mutable.PriorityQueue[PointWithDistance](PointWithDistance(Point(0, 0), 0))(
        Ordering.by[PointWithDistance, Int](_.distance).reverse
      ),
      input,
      Math.sqrt(input.size).toInt - 1
    )
  }
}
