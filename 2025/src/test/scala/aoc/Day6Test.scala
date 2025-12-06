package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day6Test extends ZIOSpecDefault:
    override def spec = suite("Day 6")(
      test("Part 1")(assertTrue(Day6.part1(InputUtils.fetchSample(2025, 6)) == 4277556L)),
      test("Part 2")(assertTrue(Day6.part2(InputUtils.fetchSample(2025, 6)) == 3263827L))
    )
