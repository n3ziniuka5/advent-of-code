package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day16 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day16.txt").getLines().toList.head
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  val hexToDec = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  sealed trait Packet {
    def version: Int
  }
  case class Literal(version: Int, value: Long)                       extends Packet
  case class Operator(typeId: Int, version: Int, value: List[Packet]) extends Packet

  @tailrec
  def parseLiteral(binString: String, acc: String = ""): (Long, String) = {
    binString.head match {
      case '1' =>
        parseLiteral(binString.drop(5), acc + binString.drop(1).take(4))
      case '0' =>
        (java.lang.Long.parseLong(acc + binString.drop(1).take(4), 2), binString.drop(5))
    }
  }

  @tailrec
  def parseContinually(binString: String, acc: List[Packet] = Nil): List[Packet] = {
    if (binString.length < 7) {
      acc
    } else {
      val (packet, remainder) = parse(binString)
      parseContinually(remainder, packet +: acc)
    }
  }

  def parse(binString: String): (Packet, String) = {
    val version = Integer.parseInt(binString.take(3), 2)
    val typeId  = Integer.parseInt(binString.slice(3, 6), 2)

    if (typeId == 4) {
      val (literalValue, remainder) = parseLiteral(binString.drop(6))
      (Literal(version, literalValue), remainder)
    } else {
      val lengthTypeId = binString.charAt(6)
      lengthTypeId match {
        case '0' =>
          val subPacketLength = Integer.parseInt(binString.drop(7).take(15), 2)
          val subPackets      = binString.drop(7).drop(15).take(subPacketLength)
          (Operator(typeId, version, parseContinually(subPackets)), binString.drop(7).drop(15).drop(subPacketLength))
        case '1' =>
          val immediatelyContainedSubPackets = Integer.parseInt(binString.drop(7).take(11), 2)
          val subPackets                     = binString.drop(7).drop(11)

          val (packets, remainder) = (1 to immediatelyContainedSubPackets).foldLeft((List.empty[Packet], subPackets)) {
            case ((packets, binString), _) =>
              val (packet, remainder) = parse(binString)
              (packet +: packets, remainder)
          }

          (Operator(typeId, version, packets), remainder)
      }
    }
  }

  def versionSum(packet: Packet): Int = {
    packet match {
      case Literal(version, _)           => version
      case Operator(_, version, packets) => version + packets.map(versionSum).sum
    }
  }

  def value(packet: Packet): Long = {
    packet match {
      case Literal(_, value) => value
      case Operator(typeId, _, packets) =>
        typeId match {
          case 0 => packets.map(value).sum
          case 1 => packets.map(value).product
          case 2 => packets.map(value).min
          case 3 => packets.map(value).max
          case 5 => if (value(packets(1)) > value(packets(0))) 1 else 0
          case 6 => if (value(packets(1)) < value(packets(0))) 1 else 0
          case 7 => if (value(packets(1)) == value(packets(0))) 1 else 0
        }
    }
  }

  def part1(lines: String): Int = {
    versionSum(parse(lines.flatMap(hexToDec))._1)
  }

  def part2(lines: String): Long = {
    value(parse(lines.flatMap(hexToDec))._1)
  }
}
