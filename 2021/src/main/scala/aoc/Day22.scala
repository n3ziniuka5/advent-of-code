package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.CollectionConverters._

object Day22 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day22.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  enum OperationType {
    case ON, OFF
  }
  case class Range(from: Int, to: Int)
  case class Operation(operationType: OperationType, xRange: Range, yRange: Range, zRange: Range)
  case class Point(x: Int, y: Int, z: Int)

  def readInput(lines: List[String]): List[Operation] = {
    lines.map { line =>
      val operationType = if (line.take(2) == "on") OperationType.ON else OperationType.OFF
      val Array(x, y, z) = line.dropWhile(_ != ' ').trim.split(',').map { rangeString =>
        val Array(from, to) = rangeString.drop(2).split("\\.\\.").map(_.toInt)
        Range(from, to)
      }

      Operation(operationType, x, y, z)
    }
  }

  def turnedOnCubes(operations: List[Operation]): Long = {
    @tailrec
    def loop(operations: List[Operation], cubes: Set[Point] = Set.empty): Long = {
      operations match {
        case head :: tail =>
          val headCubes = for {
            x <- head.xRange.from to head.xRange.to
            y <- head.yRange.from to head.yRange.to
            z <- head.zRange.from to head.zRange.to
          } yield Point(x, y, z)

          val newCubes = head.operationType match {
            case OperationType.ON  => cubes ++ headCubes
            case OperationType.OFF => cubes -- headCubes
          }
          loop(tail, newCubes)
        case Nil => cubes.size
      }
    }

    loop(operations)
  }

  def part1(lines: List[String]): Long = {
    def acceptableRange(range: Range): Boolean = {
      range.from >= -50 && range.to <= 50
    }

    val smallInput = readInput(lines).filter(op =>
      acceptableRange(op.xRange) && acceptableRange(op.yRange) && acceptableRange(op.zRange)
    )

    turnedOnCubes(smallInput)
  }

  def part2(lines: List[String]): Long = {
    // turnedOnCubes(readInput(lines))
    0L
  }
}
