package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day12Test extends ZIOSpecDefault:
    override def spec = suite("Day 12")(
      test("Part 1")(assertTrue(Day12.part1(InputUtils.fetchSample(2025, 12)) == 2)),
    )
