package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day11Test extends ZIOSpecDefault:
    override def spec = suite("Day 11")(
      test("Part 1")(assertTrue(Day11.part1(InputUtils.fetchSample(2025, 11)) == 5)),
      test("Part 2")(assertTrue(Day11.part2(InputUtils.fetchSample(2025, 11, 2)) == 2))
    )
