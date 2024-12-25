package aoc

import aoc.Common.timed
import language.experimental.namedTuples

object Day25:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 25)
        timed("Part 1", part1(lines))

    def part1(lines: List[String]): Long =
        val (locks, keys) = parse(lines)
        val pairs = for
            lock <- locks
            key  <- keys
        yield (lock, key)
        pairs.count: (lock, key) =>
            lock.zip(key).forall((h1, h2) => h1 + h2 <= 5)

    def parse(lines: List[String]): (locks: List[List[Int]], keys: List[List[Int]]) =
        val all = lines.sliding(7, 8).toList
        all.foldLeft((List.empty[List[Int]], List.empty[List[Int]])): (acc, lines) =>
            val (locks, keys) = acc
            val item          = lines.transpose.map(_.count(_ == '#') - 1)
            if lines.head.contains('.') then (locks, item +: keys)
            else (item +: locks, keys)
