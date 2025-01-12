package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day12 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day12.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def constructGraph(lines: List[String]): Map[String, List[String]] = {
    lines.foldLeft(Map.empty[String, List[String]]) { (acc, line) =>
      val Array(a, b) = line.split('-')

      def insert(map: Map[String, List[String]], node: String, edge: String) = {
        map + (node -> (edge +: map.getOrElse(node, List.empty)))
      }

      insert(insert(acc, a, b), b, a)
    }
  }

  case class Path(currentNode: String, visited: Map[String, Int] = Map.empty)

  @tailrec
  def numPaths(
    toVisit: List[Path],
    graph: Map[String, List[String]],
    maxAllowedVisits: Int,
    count: Int = 0
  ): Int = {
    toVisit match {
      case head :: tail =>
        if (head.currentNode == "end") {
          numPaths(tail, graph, maxAllowedVisits, count + 1)
        } else if (head.visited.contains(head.currentNode) && head.visited.exists(_._2 == maxAllowedVisits)) {
          numPaths(tail, graph, maxAllowedVisits, count)
        } else {
          val newVisited =
            if (head.currentNode == head.currentNode.toLowerCase)
              head.visited + (head.currentNode -> (head.visited.getOrElse(head.currentNode, 0) + 1))
            else head.visited
          numPaths(
            graph(head.currentNode).filter(_ != "start").map(Path(_, newVisited)) ++ tail,
            graph,
            maxAllowedVisits,
            count
          )
        }
      case Nil => count
    }
  }

  def part1(lines: List[String]): Int = {
    val graph = constructGraph(lines)
    numPaths(List(Path("start")), graph, 1)
  }

  def part2(lines: List[String]): Int = {
    val graph = constructGraph(lines)
    numPaths(List(Path("start")), graph, 2)
  }
}
