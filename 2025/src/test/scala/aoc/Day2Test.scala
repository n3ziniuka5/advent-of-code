package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day2Test extends ZIOSpecDefault:
    override def spec = suite("Day 2")(
      test("Part 1")(assertTrue(Day2.part1(InputUtils.fetchSample(2025, 2)) == 1227775554L)),
      test("Part 2")(assertTrue(Day2.part2(InputUtils.fetchSample(2025, 2)) == 4174379265L))
    )
