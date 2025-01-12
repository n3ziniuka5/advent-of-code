package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day4 {
  case class Input(numbers: List[Int], boards: List[Vector[Vector[Int]]])

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day4.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def readInput(lines: List[String]): Input = {
    val nonEmptylines = lines.filter(_.nonEmpty)
    val numbers       = nonEmptylines.head.split(',').map(_.toInt).toList

    val boards = nonEmptylines
      .drop(1)
      .sliding(5, 5)
      .map { boardLines =>
        boardLines.map(_.split(' ').map(_.trim).filter(_.nonEmpty).map(_.toInt).toVector).toVector
      }
      .toList

    Input(numbers, boards)
  }

  def isBoardWinning(board: Vector[Vector[Int]]): Boolean = {
    board.exists(_.forall(_ == -1)) ||
    (0 to 4).exists(col => (0 to 4).forall(row => board(row)(col) == -1))
  }

  def boardScore(latestNum: Int, board: Vector[Vector[Int]]): Int = {
    board.flatten.filter(_ != -1).sum * latestNum
  }

  def part1(lines: List[String]): Int = {
    @tailrec
    def loop(numbers: List[Int], boards: List[Vector[Vector[Int]]]): Int = {
      val head      = numbers.head
      val newBoards = boards.map(_.map(_.map(n => if (n == head) -1 else n)))

      newBoards.find(isBoardWinning) match {
        case None        => loop(numbers.tail, newBoards)
        case Some(board) => boardScore(head, board)
      }

    }

    val input = readInput(lines)

    loop(input.numbers, input.boards)
  }

  def part2(lines: List[String]): Int = {
    @tailrec
    def loop(numbers: List[Int], boards: List[Vector[Vector[Int]]]): Int = {
      val head      = numbers.head
      val newBoards = boards.map(_.map(_.map(n => if (n == head) -1 else n)))

      if (newBoards.size == 1) {
        newBoards.find(isBoardWinning) match {
          case None        => loop(numbers.tail, newBoards)
          case Some(board) => boardScore(head, board)
        }
      } else {
        val remainingBoards = newBoards.filterNot(isBoardWinning)
        loop(numbers.tail, remainingBoards)
      }
    }

    val input = readInput(lines)

    loop(input.numbers, input.boards)
  }
}
