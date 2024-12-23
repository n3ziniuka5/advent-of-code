package aoc

import aoc.Common.timed
import scala.annotation.tailrec

object Day23:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 23)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val networksOfThree = threeConnected(parse(lines))
        networksOfThree.count(_.exists(_.startsWith("t")))

    def part2(lines: List[String]): String =
        val networks = largestNetworks(parse(lines))
        networks.maxBy(_.size).toList.sorted.mkString(",")

    def threeConnected(connections: Map[String, List[String]]): Set[Set[String]] =
        val allNodes = connections.keySet
        allNodes.flatMap: node =>
            val neighbors = connections(node)
            neighbors
                .combinations(2)
                .filter: a =>
                    val pc1 = a(0)
                    val pc2 = a(1)
                    connections(pc1).contains(pc2)
                .map(_.toSet + node)
                .toSet

    def largestNetworks(connections: Map[String, List[String]]): Set[Set[String]] =
        val allNodes = connections.keySet
        allNodes.map: node =>
            val neighbors = connections(node)
            shrinkUntilAllConnected(connections, neighbors.toSet + node)

    @tailrec
    def shrinkUntilAllConnected(connections: Map[String, List[String]], network: Set[String]): Set[String] =
        val connectionCounts = network.map: node =>
            (node, (network - node).count(connections(node).contains))
        if connectionCounts.map(_._2).size == 1 then network
        else
            val leastConnected = connectionCounts.minBy(_._2)._1
            shrinkUntilAllConnected(connections, network - leastConnected)

    def parse(lines: List[String]): Map[String, List[String]] =
        lines.foldLeft(Map.empty[String, List[String]]): (acc, line) =>
            val Array(pc1, pc2) = line.split('-')
            acc + (pc1 -> (pc2 +: acc.getOrElse(pc1, List.empty[String]))) +
                (pc2   -> (pc1 +: acc.getOrElse(pc2, List.empty[String])))
