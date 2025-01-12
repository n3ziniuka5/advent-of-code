package aoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day21Test extends AnyFreeSpec with Matchers {
  val input = List(
    "Player 1 starting position: 4",
    "Player 2 starting position: 8"
  )

  "Part 1" in {
    Day21.part1(input) mustBe 739785
  }

  "Part 2" in {
    Day21.part2(input) mustBe 444356092776315L
  }
}
