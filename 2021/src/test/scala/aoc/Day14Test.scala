package aoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day14Test extends AnyFreeSpec with Matchers {
  val input = List(
    "NNCB",
    "",
    "CH -> B",
    "HH -> N",
    "CB -> H",
    "NH -> C",
    "HB -> C",
    "HC -> B",
    "HN -> C",
    "NN -> C",
    "BH -> H",
    "NC -> B",
    "NB -> B",
    "BN -> B",
    "BB -> N",
    "BC -> B",
    "CC -> N",
    "CN -> C"
  )

  "Part 1" in {
    Day14.part1(input) mustBe 1588
  }

  "Part 2" in {
    Day14.part2(input) mustBe 2188189693529L
  }
}
