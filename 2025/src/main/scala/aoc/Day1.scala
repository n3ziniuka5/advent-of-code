package aoc

import aoc.Common.timed

object Day1:
    enum Direction:
        case Left, Right

    case class DialNode(value: Int, var left: DialNode, var right: DialNode):
        def go(direction: Direction): DialNode =
            direction match
                case Direction.Left  => left
                case Direction.Right => right

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 1)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parseLines(lines: List[String]): List[(Direction, Int)] =
        lines.map: line =>
            val direction = line.head match
                case 'L' => Direction.Left
                case 'R' => Direction.Right
            val distance = line.tail.toInt
            (direction, distance)

    def initialDial(): DialNode =
        val zero            = DialNode(0, null, null)
        var start: DialNode = null
        var prevNode        = zero
        (1 to 99).foreach: i =>
            val newNode = DialNode(i, prevNode, null)
            prevNode.right = newNode
            if i == 50 then start = newNode
            if i == 99 then
                newNode.right = zero
                zero.left = newNode
            prevNode = newNode
        start

    def part1(lines: List[String]): Long =
        val instructions = parseLines(lines)
        val start        = initialDial()

        def loop(currentPosition: DialNode, remainingInstructions: List[(Direction, Int)], result: Long): Long =
            remainingInstructions match
                case head :: tail =>
                    val newPosition = (1 to head._2 % 100).foldLeft(currentPosition): (pos, _) =>
                        pos.go(head._1)
                    val newResult = if newPosition.value == 0 then result + 1 else result
                    loop(newPosition, tail, newResult)
                case Nil => result

        loop(start, instructions, 0L)

    def part2(lines: List[String]): Long =
        val instructions = parseLines(lines)
        val start        = initialDial()

        def loop(currentPosition: DialNode, remainingInstructions: List[(Direction, Int)], result: Long): Long =
            remainingInstructions match
                case head :: tail =>
                    val newPosition = (1 to head._2 % 100).foldLeft((currentPosition, 0L)): (s, _) =>
                        val (pos, res) = s
                        val nextPos    = pos.go(head._1)
                        val newRes     = if nextPos.value == 0 then res + 1 else res
                        (nextPos, newRes)
                    val newResult = result + newPosition._2 + head._2 / 100
                    loop(newPosition._1, tail, newResult)
                case Nil => result

        loop(start, instructions, 0L)
