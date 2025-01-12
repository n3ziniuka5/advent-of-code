package aoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day6Test extends AnyFreeSpec with Matchers {
  val input = List(
    "3,4,3,1,2"
  )

  "Part 1" in {
    Day6.part1(input) mustBe 5934
  }

  "Part 2" in {
    Day6.part2(input) mustBe 26984457539L
  }
}
