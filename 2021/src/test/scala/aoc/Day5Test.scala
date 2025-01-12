package aoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day5Test extends AnyFreeSpec with Matchers {
  val input = List(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )

  "Part 1" in {
    Day5.part1(input) mustBe 5
  }

  "Part 2" in {
    Day5.part2(input) mustBe 12
  }
}
