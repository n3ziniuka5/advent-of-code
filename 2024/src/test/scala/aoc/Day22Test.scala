package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day22Test extends ZIOSpecDefault:
    override def spec = suite("Day 22")(
      test("Part 1")(assertTrue(Day22.part1(InputUtils.fetchSample(2024, 22, 2)) == 37327623)),
      test("Part 2")(assertTrue(Day22.part2(InputUtils.fetchSample(2024, 22, 6)) == 23))
    )
