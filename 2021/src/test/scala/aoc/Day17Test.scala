package aoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day17Test extends AnyFreeSpec with Matchers {
  val input = List(
    "target area: x=20..30, y=-10..-5"
  )

  "Part 1" in {
    Day17.part1(input) mustBe 45
  }

  "Part 2" in {
    Day17.part2(input) mustBe 112
  }
}
