package aoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day15Test extends AnyFreeSpec with Matchers {
  val input = List(
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581"
  )

  "Part 1" in {
    Day15.part1(input) mustBe 40
  }

  "Part 2" in {
    Day15.part2(input) mustBe 315
  }
}
