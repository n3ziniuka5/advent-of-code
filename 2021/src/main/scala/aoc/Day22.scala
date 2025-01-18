package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

object Day22:
    enum OperationType:
        case ON, OFF
    case class Range(from: Long, to: Long)
    case class Square(xRange: Range, yRange: Range):
        def isValid: Boolean = xRange.from <= xRange.to && yRange.from <= yRange.to
        def area: Long       = (xRange.to - xRange.from + 1) * (yRange.to - yRange.from + 1)
    case class Operation(operationType: OperationType, square: Square, zRange: Range)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2021, 22)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val operations = readInput(lines, smallInput = true)
        turnedOnCubes(operations)

    def part2(lines: List[String]): Long =
        val operations = readInput(lines, smallInput = false)
        turnedOnCubes(operations)

    def turnedOnCubes(operations: List[Operation]): Long =
        val minZ = operations.map(_.zRange.from).min
        val maxZ = operations.map(_.zRange.to).max

        (minZ to maxZ).par
            .map: z =>
                val reversedAndFiltered = operations.reverse
                    .filter(op => z >= op.zRange.from && z <= op.zRange.to)
                reduceOperations(reversedAndFiltered)
            .sum

    def reduceOperations(operations: List[Operation]): Long =
        @tailrec
        def loop(operations: List[Operation], processed: List[Operation], turnedOn: Long): Long =
            operations match
                case head :: tail =>
                    val additionalTurnedOn =
                        if head.operationType == OperationType.OFF then 0L
                        else
                            processed
                                .foldLeft(List(head.square)): (squares, processedOp) =>
                                    squares.flatMap(square => subtract(square, processedOp.square))
                                .map(_.area)
                                .sum
                    loop(tail, head +: processed, turnedOn + additionalTurnedOn)
                case Nil => turnedOn

        loop(operations, Nil, 0L)

    def subtract(from: Square, term: Square): List[Square] =
        if term.xRange.from <= from.xRange.from && term.xRange.to >= from.xRange.to && term.yRange.from <= from.yRange.from && term.yRange.to >= from.yRange.to
        then Nil
        else if term.xRange.from > from.xRange.to || term.xRange.to < from.xRange.from || term.yRange.from > from.yRange.to || term.yRange.to < from.yRange.from
        then List(from)
        else
            val topSquare    = Square(from.xRange, Range(from.yRange.from, term.yRange.from - 1))
            val bottomSquare = Square(from.xRange, Range(term.yRange.to + 1, from.yRange.to))

            val sideYRange =
                Range(math.max(term.yRange.from, from.yRange.from), math.min(term.yRange.to, from.yRange.to))
            val leftSquare  = Square(Range(from.xRange.from, term.xRange.from - 1), sideYRange)
            val rightSquare = Square(Range(term.xRange.to + 1, from.xRange.to), sideYRange)

            List(topSquare, bottomSquare, leftSquare, rightSquare).filter(_.isValid)

    def readInput(lines: List[String], smallInput: Boolean): List[Operation] =
        def smallInputRange(range: Range): Boolean =
            range.from >= -50 && range.to <= 50

        val baseOperations = lines.map { line =>
            val operationType = if line.take(2) == "on" then OperationType.ON else OperationType.OFF
            val Array(x, y, z) = line.dropWhile(_ != ' ').trim.split(',').map { rangeString =>
                val Array(from, to) = rangeString.drop(2).split("\\.\\.").map(_.toInt)
                Range(from, to)
            }

            Operation(operationType, Square(x, y), z)
        }

        if smallInput then
            baseOperations.filter: op =>
                smallInputRange(op.square.xRange) && smallInputRange(op.square.yRange) && smallInputRange(op.zRange)
        else baseOperations
