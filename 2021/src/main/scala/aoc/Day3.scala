package aoc

import aoc.Common.timed
import scala.annotation.tailrec
import scala.io.Source

object Day3 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day3.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def invertBinaryNum(num: String): String = {
    num.map {
      case '1' => '0'
      case '0' => '1'
    }
  }

  def binToDec(num: String): Int = {
    Integer.parseInt(num, 2)
  }

  def part1(lines: List[String]): Int = {
    val numLength = lines.head.length
    val numLines  = lines.length

    val gammaRate = (0 until numLength)
      .map { i =>
        val ones = lines.count(num => num.charAt(i) == '1')
        if (ones > numLines / 2) '1' else '0'
      }
      .mkString("")

    binToDec(gammaRate) * binToDec(invertBinaryNum(gammaRate))
  }

  @tailrec
  def filterUntilOneRemains(lines: List[String], useCharWhen1MostCommon: Char, i: Int = 0): String = {
    lines match {
      case head :: Nil => head
      case _ =>
        val ones   = lines.count(_.charAt(i) == '1')
        val zeroes = lines.length - ones
        val filterBy =
          if (ones >= zeroes) useCharWhen1MostCommon else invertBinaryNum(useCharWhen1MostCommon.toString).head
        val newLines = lines.filter(_.charAt(i) == filterBy)
        filterUntilOneRemains(newLines, useCharWhen1MostCommon, i + 1)
    }
  }

  def part2(lines: List[String]): Int = {
    val oxygenGeneratorRating = filterUntilOneRemains(lines, '1')
    val co2ScrubberRating     = filterUntilOneRemains(lines, '0')

    binToDec(oxygenGeneratorRating) * binToDec(co2ScrubberRating)
  }
}
