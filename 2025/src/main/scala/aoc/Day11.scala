package aoc

import aoc.Common.timed

object Day11:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 11)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parse(lines: List[String]): Map[String, Vector[String]] =
        lines
            .map:
                case s"$name: $outputs" =>
                    val outputList = outputs.split(" ").toVector
                    (name, outputList)
            .toMap

    def numberOfPaths(from: String, to: String, graph: Map[String, Vector[String]]): Long =
        val pathCache = collection.mutable.HashMap.empty[(String, String), Long]
        def compute(from: String, to: String): Long =
            pathCache.get((from, to)) match
                case Some(cached) => cached
                case None =>
                    val result =
                        if !graph.contains(from) then 0
                        else
                            val directions = graph(from)
                            if directions.contains(to) then 1
                            else directions.map(newFrom => compute(newFrom, to)).sum

                    val _ = pathCache.put((from, to), result)
                    result

        compute(from, to)

    def part1(lines: List[String]): Long =
        val paths = parse(lines)
        numberOfPaths("you", "out", paths)

    def part2(lines: List[String]): Long =
        val paths = parse(lines)

        val p1 = numberOfPaths("svr", "fft", paths)
        val p2 = numberOfPaths("fft", "dac", paths)
        val p3 = numberOfPaths("dac", "out", paths)

        List(p1, p2, p3).product
