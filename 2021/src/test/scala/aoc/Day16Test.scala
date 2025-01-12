package aoc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day16Test extends AnyFreeSpec with Matchers {
  val input = List(
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581"
  )

  "Part 1" in {
    Day16.part1("D2FE28") mustBe 6
    Day16.part1("8A004A801A8002F478") mustBe 16
    Day16.part1("620080001611562C8802118E34") mustBe 12
    Day16.part1("C0015000016115A2E0802F182340") mustBe 23
    Day16.part1("A0016C880162017C3686B18A3D4780") mustBe 31
  }

  "Part 2" in {
    Day16.part2("C200B40A82") mustBe 3
    Day16.part2("04005AC33890") mustBe 54
    Day16.part2("880086C3E88112") mustBe 7
    Day16.part2("CE00C43D881120") mustBe 9
    Day16.part2("D8005AC2A8F0") mustBe 1
    Day16.part2("F600BC2D8F") mustBe 0
    Day16.part2("9C005AC2F8F0") mustBe 0
    Day16.part2("9C0141080250320F1802104A08") mustBe 1
  }
}
