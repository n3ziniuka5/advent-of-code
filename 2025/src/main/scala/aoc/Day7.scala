package aoc

import aoc.Common.timed

object Day7:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 7)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val map                = Map2d.fromLines(lines)
        val onlySignificantMap = map.underlying.filter(_._2 != '.')
        val start              = onlySignificantMap.find(_._2 == 'S').get._1
        val firstSplit         = (1L to map.maxY).find(y => onlySignificantMap.get(Point(start.x, y)).contains('^')).get

        def loop(remaining: List[Point], splitAt: List[Point]): Long =
            remaining match
                case Nil => splitAt.size
                case head :: tail =>
                    val maybeLowestPossibleSplit = splitAt.find(p => p.x + 1 == head.x || p.x - 1 == head.x)
                    maybeLowestPossibleSplit match
                        case Some(lowestPossibleSplit) =>
                            val maybeLowestBlock =
                                (head.y - 1 to 1 by -1).find(y =>
                                    onlySignificantMap.get(Point(head.x, y)).contains('^')
                                )

                            maybeLowestBlock match
                                case Some(lowestBlock) if lowestBlock > lowestPossibleSplit.y => loop(tail, splitAt)
                                case _ => loop(tail, head +: splitAt)
                        case None => loop(tail, splitAt)

        loop(onlySignificantMap.keySet.toList.sortBy(_.y), List(Point(start.x, firstSplit)))

    def part2(lines: List[String]): Long =
        val map                = Map2d.fromLines(lines)
        val onlySignificantMap = map.underlying.filter(_._2 != '.')
        val start              = onlySignificantMap.find(_._2 == 'S').get._1
        val firstSplit         = (1L to map.maxY).find(y => onlySignificantMap.get(Point(start.x, y)).contains('^')).get

        def loop(remaining: List[Point], splitAt: Map[Point, Long]): Long =
            remaining match
                case Nil =>
                    (0L to map.maxX).map(x => splitAt.get(Point(x, map.maxY)).getOrElse(0L)).sum
                case head :: tail =>
                    val splits =
                        splitAt.filter(p => (p._1.x - 1 == head.x | p._1.x + 1 == head.x) && p._1.y != map.maxY)

                    val sums = splits
                        .flatMap: split =>
                            val maybeLowestBlock =
                                (head.y - 1 to 1 by -1).find(y =>
                                    onlySignificantMap.get(Point(head.x, y)).contains('^')
                                )
                            maybeLowestBlock match
                                case Some(lowestBlock) if lowestBlock > split._1.y =>
                                    None
                                case _ =>
                                    Some(split._2)
                        .sum

                    if sums != 0 then loop(tail, splitAt + (head -> sums)) else loop(tail, splitAt)

        val bottomRow = (0L to map.maxX).map(x => Point(x, map.maxY))
        loop(
          onlySignificantMap.keySet.toList.sortBy(_.y) ++ bottomRow,
          Map(Point(start.x, firstSplit) -> 1L)
        )
