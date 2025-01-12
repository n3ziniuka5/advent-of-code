package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day25 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day25.txt").getLines().toList
    timed("Part 1", part1(lines))
  }

  case class Input(map: Map[Point, Char], width: Int, height: Int)
  case class Point(x: Int, y: Int)

  def readInput(lines: List[String]): Input = {
    val map = lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.filter(_._1 != '.').map { case (char, x) =>
        Point(x, y) -> char
      }
    }.toMap

    val width  = lines.head.size
    val height = lines.size

    Input(map, width, height)
  }

  def performStep(map: Map[Point, Char], width: Int, height: Int): Map[Point, Char] = {
    def move(map: Map[Point, Char], target: Char, pointTransformation: Point => Point): Map[Point, Char] = {
      map.map { case (point, char) =>
        val newPoint = char match {
          case `target` => pointTransformation(point)
          case _        => point
        }
        if (map.contains(newPoint)) {
          point -> char
        } else {
          newPoint -> char
        }
      }
    }

    val eastFacingMove = move(map, '>', p => p.copy(x = (p.x + 1) % width))
    move(eastFacingMove, 'v', p => p.copy(y = (p.y + 1) % height))
  }

  @tailrec
  def stepsUntilNotMoving(input: Input, stepsTaken: Int = 1): Int = {
    val newMap = performStep(input.map, input.width, input.height)
    if (newMap == input.map) {
      stepsTaken
    } else {
      stepsUntilNotMoving(input.copy(map = newMap), stepsTaken + 1)
    }
  }

  def part1(lines: List[String]): Int = {
    stepsUntilNotMoving(readInput(lines))
  }
}
