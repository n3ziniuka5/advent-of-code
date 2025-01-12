package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day13 {
  case class Position(x: Int, y: Int)
  sealed trait Fold
  case class AlongX(x: Int) extends Fold
  case class AlongY(y: Int) extends Fold

  case class Input(positions: Set[Position], folds: List[Fold])

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day13.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def readInput(lines: List[String], acc: Input = Input(Set.empty, List.empty)): Input = {
    lines match {
      case head :: tail =>
        if (head.isEmpty) {
          readInput(tail, acc)
        } else if (head.contains(',')) {
          val Array(x, y) = head.split(',').map(_.toInt)
          readInput(tail, acc.copy(positions = acc.positions + Position(x, y)))
        } else {
          val Array(foldType, point) = head.split('=')
          val fold = foldType match {
            case "fold along x" => AlongX(point.toInt)
            case "fold along y" => AlongY(point.toInt)
          }
          readInput(tail, acc.copy(folds = fold +: acc.folds))
        }
      case Nil => acc.copy(folds = acc.folds.reverse)
    }
  }

  def foldOnce(positions: Set[Position], fold: Fold): Set[Position] = {
    fold match {
      case AlongX(x) =>
        val (remaining, cutOff) = positions.partition(_.x < x)

        remaining ++ cutOff.map { p =>
          p.copy(x = x - (p.x - x))
        }
      case AlongY(y) =>
        val (remaining, cutOff) = positions.partition(_.y < y)

        remaining ++ cutOff.map { p =>
          p.copy(y = y - (p.y - y))
        }
    }
  }

  @tailrec
  def fold(folds: List[Fold], positions: Set[Position]): Set[Position] = {
    folds match {
      case head :: tail => fold(tail, foldOnce(positions, head))
      case Nil          => positions
    }
  }

  def part1(lines: List[String]): Int = {
    val input = readInput(lines)
    foldOnce(input.positions, input.folds.head).size
  }

  def part2(lines: List[String]): Unit = {
    val input  = readInput(lines)
    val folded = fold(input.folds, input.positions)

    val maxX = folded.maxBy(_.x).x
    val maxY = folded.maxBy(_.y).y

    val rendered = for {
      y <- 0 to maxY
      x <- 0 to maxX
    } yield {
      val pointChar = if (folded.contains(Position(x, y))) "#" else " "
      val newLine   = if (x == maxX) "\n" else ""
      pointChar + newLine
    }

    println(rendered.mkString)
  }
}
