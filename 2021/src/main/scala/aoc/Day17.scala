package aoc

import aoc.Common.timed

import scala.io.Source

object Day17 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day17.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  case class Range(from: Int, to: Int)
  case class Point(x: Int, y: Int)

  def readInput(lines: List[String]): (Range, Range) = {
    val Array(xRange, yRange) = lines.head.drop(13).split(',').map(_.trim).map { a =>
      val Array(from, to) = a.drop(2).split("\\.\\.").map(_.toInt)
      Range(from, to)
    }
    (xRange, yRange)
  }

  def allPossibleXVelocitiesWithSteps(xRange: Range, yRange: Range): Set[(Int, Int)] = {
    (0 to xRange.to).flatMap { velocity =>
      val steps = (1 to velocity).filter { steps =>
        val traveled = (velocity * steps) - (((steps - 1) * (0 + steps)) / 2)
        traveled >= xRange.from && traveled <= xRange.to
      }
      val additionalStepsIfBallStopped = if (steps.contains(velocity)) {
        steps ++ ((velocity + 1) to Math.abs((yRange.from * 2)))
      } else {
        steps
      }
      additionalStepsIfBallStopped.map(s => (velocity, s))
    }.toSet
  }

  def findAllVelocities(xRange: Range, yRange: Range): Set[(Int, Int)] = {
    (for {
      steps   <- allPossibleXVelocitiesWithSteps(xRange, yRange)
      targetY <- yRange.from to yRange.to
    } yield (steps, targetY)).flatMap { case ((xVelocity, steps), targetY) =>
      val velocity = BigDecimal(targetY + (((steps - 1) * (0 + steps)) / 2)) / steps
      if (velocity.isWhole) {
        Some((xVelocity, velocity.intValue))
      } else {
        None
      }
    }
  }

  def maxHeight(y: Int): Int = {
    (1 + y) * y / 2
  }

  def part1(lines: List[String]): Int = {
    val (xRange, yRange) = readInput(lines)
    val allVelocities    = findAllVelocities(xRange, yRange)

    maxHeight(allVelocities.maxBy(_._2)._2)
  }

  def part2(lines: List[String]): Int = {
    val (xRange, yRange) = readInput(lines)

    findAllVelocities(xRange, yRange).size
  }
}
