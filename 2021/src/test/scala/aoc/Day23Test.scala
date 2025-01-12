package aoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day23Test extends AnyFreeSpec with Matchers {
  val input = List(
    "#############",
    "#...........#",
    "###B#C#B#D###",
    "  #A#D#C#A#",
    "  #########"
  )

  "Part 1" in {
    Day23.part1(input) mustBe 12521
  }

  "Part 2" in {
    Day23.part2(input) mustBe 44169
  }
}
