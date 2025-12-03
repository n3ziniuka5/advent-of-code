package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day3Test extends ZIOSpecDefault:
    override def spec = suite("Day 3")(
      test("Part 1")(assertTrue(Day3.part1(InputUtils.fetchSample(2025, 3)) == 357)),
      test("Part 2")(assertTrue(Day3.part2(InputUtils.fetchSample(2025, 3)) == 3121910778619L))
    )
