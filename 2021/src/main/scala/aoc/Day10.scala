package aoc

import aoc.Common.timed

import scala.io.Source

object Day10 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day10.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def firstInvalidCharacterWithRemainder(str: String, openings: List[Char] = Nil): (Option[Char], List[Char]) = {
    str.headOption match {
      case None => (None, openings)
      case Some(head) =>
        head match {
          case opening @ ('(' | '[' | '{' | '<') => firstInvalidCharacterWithRemainder(str.tail, opening +: openings)
          case closing =>
            (head, openings.headOption) match {
              case (')', Some('(')) => firstInvalidCharacterWithRemainder(str.tail, openings.tail)
              case (']', Some('[')) => firstInvalidCharacterWithRemainder(str.tail, openings.tail)
              case ('}', Some('{')) => firstInvalidCharacterWithRemainder(str.tail, openings.tail)
              case ('>', Some('<')) => firstInvalidCharacterWithRemainder(str.tail, openings.tail)
              case _                => (Some(head), Nil)
            }

        }
    }
  }

  def part1(lines: List[String]): Int = {
    lines
      .map(firstInvalidCharacterWithRemainder(_))
      .flatMap(_._1)
      .map {
        case ')' => 3
        case ']' => 57
        case '}' => 1197
        case '>' => 25137
      }
      .sum
  }

  def part2(lines: List[String]): Long = {
    val sortedScores = lines
      .map(firstInvalidCharacterWithRemainder(_))
      .filter(_._1.isEmpty)
      .map(_._2)
      .map { openings =>
        openings.foldLeft(0L) { (accum, char) =>
          val toAdd = char match {
            case '(' => 1
            case '[' => 2
            case '{' => 3
            case '<' => 4
          }
          accum * 5 + toAdd
        }
      }
      .sorted

    sortedScores(sortedScores.length / 2)
  }
}
