package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day9 {
  case class Position(x: Int, y: Int) {
    def neighbours: Set[Position] =
      Set(
        Position(x - 1, y),
        Position(x + 1, y),
        Position(x, y - 1),
        Position(x, y + 1)
      )
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day9.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def constructMap(lines: List[String]): Map[Position, Int] = {
    lines.zipWithIndex.foldLeft(Map.empty[Position, Int]) { case (acc, (l, y)) =>
      acc ++ l.zipWithIndex.map { case (height, x) =>
        Position(x, y) -> height.toString.toInt
      }.toMap
    }
  }

  def part1(lines: List[String]): Int = {
    val heightMap = constructMap(lines)
    heightMap
      .filter { case (pos, height) =>
        pos.neighbours.forall { neighbor =>
          heightMap.get(neighbor) match {
            case Some(neighborHeight) => height < neighborHeight
            case None                 => true
          }
        }
      }
      .map(_._2 + 1)
      .sum
  }

  @tailrec
  def measureBasin(
    toVisit: List[Position],
    heightMap: Map[Position, Int],
    visited: Set[Position],
    size: Int
  ): (Int, Set[Position]) = {
    toVisit match {
      case head :: tail =>
        if (visited.contains(head) || !heightMap.contains(head) || heightMap(head) == 9) {
          measureBasin(tail, heightMap, visited + head, size)
        } else {
          measureBasin(head.neighbours.toList ++ tail, heightMap, visited + head, size + 1)
        }
      case Nil => (size, visited)
    }
  }

  @tailrec
  def findBasins(
    toVisit: List[Position],
    heightMap: Map[Position, Int],
    visited: Set[Position] = Set.empty,
    basins: List[Int] = Nil
  ): List[Int] = {
    toVisit match {
      case head :: tail =>
        if (visited.contains(head) || heightMap(head) == 9) {
          findBasins(tail, heightMap, visited, basins)
        } else {
          val newBasin = measureBasin(List(head), heightMap, Set.empty, 0)

          findBasins(tail, heightMap, visited ++ newBasin._2, newBasin._1 +: basins)
        }
      case Nil => basins
    }
  }

  def part2(lines: List[String]): Int = {
    val heightMap = constructMap(lines)
    findBasins(heightMap.keySet.toList, heightMap)
      .sorted(implicitly[Ordering[Int]].reverse)
      .take(3)
      .product
  }
}
