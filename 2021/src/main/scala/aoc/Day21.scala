package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day21 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day21.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  case class Input(player1: Int, player2: Int)

  def readInput(lines: List[String]): Input = {
    val positions = lines.map(_.last.toString.toInt)
    Input(positions(0), positions(1))
  }

  def modSpace(space: Int): Int = {
    val a = space % 10

    if (a == 0) {
      10
    } else {
      a
    }
  }

  def part1(lines: List[String]): Int = {
    val input = readInput(lines)

    val player1Space1Scores = LazyList.continually(()).zipWithIndex.scanLeft(1) { case (currentSpace, (_, step)) =>
      val multiplier = (step * 3) + 1
      val reduction  = (multiplier / 50) * 300
      val diceRoll   = 6 * multiplier - reduction
      modSpace(currentSpace + diceRoll)
    }

    val player2Space1Scores = LazyList.continually(()).zipWithIndex.scanLeft(1) { case (currentSpace, (_, step)) =>
      val multiplier = (step * 6) + 5
      val reduction  = (multiplier / 100) * 300
      val diceRoll   = 3 * multiplier - reduction
      modSpace(currentSpace + diceRoll)
    }

    def points(scores: LazyList[Int], startingSpace: Int): LazyList[(Int, Int)] = {
      scores
        .map(space => modSpace(space + (startingSpace - 1)))
        .drop(1)
        .scanLeft(0)(_ + _)
        .zipWithIndex
    }

    val p1Points = points(player1Space1Scores, input.player1)
    val p2Points = points(player2Space1Scores, input.player2)

    val p1WinsAt = p1Points.find(_._1 >= 1000).get
    val p2WinsAt = p2Points.find(_._1 >= 1000).get

    if (p1WinsAt._2 <= p2WinsAt._2) {
      val totalRolls   = p1WinsAt._2 * 2 * 3 - 3
      val losingPoints = p2Points.drop(p1WinsAt._2 - 1).head._1

      totalRolls * losingPoints
    } else {
      val totalRolls   = p2WinsAt._2 * 2 * 3
      val losingPoints = p1Points.drop(p2WinsAt._2).head._1

      totalRolls * losingPoints
    }
  }

  // this GameState is only for Part 2
  case class GameState(player1Space: Int, player2Space: Int, p1Points: Int, p2Points: Int, p1MovesNext: Boolean) {
    def isWinning: Boolean = {
      p1Points >= 21 || p2Points >= 21
    }

    def applyRoll(roll: Int): GameState = {
      if (p1MovesNext) {
        val newSpace = modSpace(player1Space + roll)
        copy(p1Points = p1Points + newSpace, player1Space = newSpace, p1MovesNext = false)
      } else {
        val newSpace = modSpace(player2Space + roll)
        copy(p2Points = p2Points + newSpace, player2Space = newSpace, p1MovesNext = true)
      }
    }
  }

  def numberOfWinningUniverses(p1StartingSpace: Int, p2StartingSpace: Int): Long = {
    val allPossibleStates = for {
      p1Points    <- 30 to 0 by -1
      p2Points    <- 30 to 0 by -1
      p1Space     <- 10 to 1 by -1
      p2Space     <- 10 to 1 by -1
      p1MovesNext <- List(true, false)
    } yield GameState(p1Space, p2Space, p1Points, p2Points, p1MovesNext)

    @tailrec
    def loop(
      states: List[GameState],
      cache: Map[GameState, (Long, Long)] = Map.empty
    ): Map[GameState, (Long, Long)] = {
      states match {
        case head :: tail =>
          if (head.isWinning) {
            val (p1Wins, p2Wins) = if (head.p1Points >= head.p2Points) (1, 0) else (0, 1)
            loop(tail, cache + (head -> (p1Wins, p2Wins)))
          } else {
            val wins = (for {
              dice1 <- List(1, 2, 3)
              dice2 <- List(1, 2, 3)
              dice3 <- List(1, 2, 3)
            } yield dice1 + dice2 + dice3)
              .map(head.applyRoll)
              .map(cache(_))
              .foldLeft((0L, 0L)) { case ((accA, accB), (a, b)) =>
                (accA + a, accB + b)
              }

            loop(tail, cache + (head -> wins))
          }

        case Nil => cache
      }
    }

    val targetGameState = GameState(p1StartingSpace, p2StartingSpace, 0, 0, true)
    val result          = loop(allPossibleStates.appended(targetGameState).toList)
    val totalWins       = result(targetGameState)

    Math.max(totalWins._1, totalWins._2)
  }

  def part2(lines: List[String]): Long = {
    val input = readInput(lines)
    numberOfWinningUniverses(input.player1, input.player2)
  }
}
