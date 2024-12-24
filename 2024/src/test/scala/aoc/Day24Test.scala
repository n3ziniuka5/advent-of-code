package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day24Test extends ZIOSpecDefault:
    override def spec = suite("Day 24")(
      test("Part 1")(assertTrue(Day24.part1(InputUtils.fetchSample(2024, 24, 2)) == 2024))
    )
