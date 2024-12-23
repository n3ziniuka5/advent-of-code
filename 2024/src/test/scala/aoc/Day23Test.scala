package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day23Test extends ZIOSpecDefault:
    override def spec = suite("Day 23")(
      test("Part 1")(assertTrue(Day23.part1(InputUtils.fetchSample(2024, 23)) == 7)),
      test("Part 2")(assertTrue(Day23.part2(InputUtils.fetchSample(2024, 23)) == "co,de,ka,ta"))
    )
