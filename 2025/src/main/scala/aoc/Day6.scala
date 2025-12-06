package aoc

import aoc.Common.timed

object Day6:
    case class Problem(numbers: Iterable[Long], operation: String):
        def result = operation match
            case "+" => numbers.sum
            case "*" => numbers.product

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 6)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parseP1(lines: List[String]): List[Problem] =
        val linesVector = lines.toVector
        val numbers     = linesVector.init.map(_.split(" ").filter(_.trim().nonEmpty).map(_.toLong)).transpose
        val operations  = linesVector.last.split(" ").toVector.filter(_.trim().nonEmpty)

        numbers.indices
            .map: i =>
                Problem(numbers(i), operations(i))
            .toList

    def parseP2(lines: List[String]): List[Problem] =
        val linesVector       = lines.toVector
        val withoutOperations = linesVector.init
        val operations        = linesVector.last.split(" ").toVector.filter(_.trim().nonEmpty)

        val linesN   = withoutOperations.size
        val columnsN = withoutOperations.map(_.length()).max

        def loop(line: Int, col: Int, building: String, numbers: List[Long], problems: List[Problem]): List[Problem] =
            if col == columnsN then
                val newProblem = Problem(numbers, operations(problems.size))
                newProblem +: problems
            else if line == linesN then
                if building.trim == "" then
                    val newProblem = Problem(numbers, operations(problems.size))
                    loop(0, col + 1, "", Nil, newProblem +: problems)
                else loop(0, col + 1, "", building.trim().toLong +: numbers, problems)
            else loop(line + 1, col, building + withoutOperations(line).charAt(col), numbers, problems)

        loop(0, 0, "", Nil, Nil)

    def part1(lines: List[String]): Long =
        val problems = parseP1(lines)
        problems.map(_.result).sum

    def part2(lines: List[String]): Long =
        val problems = parseP2(lines)
        problems.map(_.result).sum
