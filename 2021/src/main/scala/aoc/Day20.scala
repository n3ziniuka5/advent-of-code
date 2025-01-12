package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day20 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day20.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  case class Point(x: Int, y: Int)
  case class Input(charSelector: String, picture: Map[Point, Char])

  def readInput(lines: List[String]): Input = {
    val charSelector = lines.head

    val picture = lines
      .drop(2)
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (char, x) =>
          Point(x, y) -> char
        }
      }
      .toMap

    Input(charSelector, picture)
  }

  def charFromInputPicture(charSelector: String, picture: Map[Point, Char], point: Point, infinityChar: Char): Char = {
    val charSequence = for {
      y <- -1 to 1
      x <- -1 to 1
      p = Point(point.x + x, point.y + y)
      char = picture.getOrElse(p, infinityChar) match {
        case '#' => '1'
        case '.' => '0'
      }
    } yield char

    val binaryString = charSequence.mkString
    charSelector.charAt(Integer.parseInt(binaryString, 2))
  }

  def transformPicture(charSelector: String, picture: Map[Point, Char], infinityChar: Char): Map[Point, Char] = {
    val pictureVector = picture.toVector
    val orderedByX    = pictureVector.sortBy(_._1.x)
    val minX          = orderedByX.head._1.x
    val maxX          = orderedByX.last._1.x

    val orderedByY = pictureVector.sortBy(_._1.y)
    val minY       = orderedByY.head._1.y
    val maxY       = orderedByY.last._1.y

    (for {
      y <- (minY - 1) to (maxY + 1)
      x <- (minX - 1) to (maxX + 1)
      point = Point(x, y)
    } yield point -> charFromInputPicture(charSelector, picture, point, infinityChar)).toMap
  }

  @tailrec
  def transformNTimes(n: Int, charSelector: String, picture: Map[Point, Char], infinityChar: Char = '.'): Int = {
    if (n == 0) {
      picture.count(_._2 == '#')
    } else {
      val newPicture = transformPicture(charSelector, picture, infinityChar)
      val newInfinityChar = if (charSelector.head == '#') {
        infinityChar match {
          case '#' => '.'
          case '.' => '#'
        }
      } else infinityChar

      transformNTimes(n - 1, charSelector, newPicture, newInfinityChar)
    }
  }

  def part1(lines: List[String]): Int = {
    val input = readInput(lines)
    transformNTimes(2, input.charSelector, input.picture)
  }

  def part2(lines: List[String]): Int = {
    val input = readInput(lines)
    transformNTimes(50, input.charSelector, input.picture)
  }
}
