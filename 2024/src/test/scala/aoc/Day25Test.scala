package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day25Test extends ZIOSpecDefault:
    override def spec = suite("Day 25")(
      test("Part 1")(assertTrue(Day25.part1(InputUtils.fetchSample(2024, 25)) == 3))
    )
