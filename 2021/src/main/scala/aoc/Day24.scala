package aoc

import aoc.Common.timed

object Day24:
    case class Operation(val t1: String, val t2: String)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2021, 24)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): String =
        val chunks = parse(lines)
        findAnswer(chunks.reverse, 0, Ordering[Long]).getOrElse("not found")

    def part2(lines: List[String]): String =
        val chunks = parse(lines)
        findAnswer(chunks.reverse, 0, Ordering[Long].reverse).getOrElse("not found")

    val findAnswerCache = collection.mutable.Map.empty[(Int, Long, Ordering[Long]), Option[String]]
    def findAnswer(chunks: List[List[Operation]], requiredZ: Long, longOrder: Ordering[Long]): Option[String] =
        val cacheKey = (chunks.length, requiredZ, longOrder)
        lazy val result =
            val fromHigherNumber =
                if chunks.head(3).t2 == "26" then
                    (1 to 9).toList.map: w =>
                        (w, requiredZ * 26 + w + (chunks.head(4).t2.toLong * -1))
                else Nil

            val fromSimilarNumber =
                if chunks.head(3).t2 == "26" then
                    val start = requiredZ - 12
                    val end   = requiredZ + 13
                    val w     = requiredZ - start - chunks.head(14).t2.toLong

                    val addX = chunks.head(4).t2.toLong

                    (start to end).toList
                        .map: z =>
                            (w, z)
                        .filter((w, z) => w >= 1 && w <= 9 && z >= 0)
                        .filter((w, z) => z % 26 + addX != w)
                        .map((w, z) => (w.toInt, z))
                else Nil

            val fromSmallerNumber =
                if chunks.head(3).t2 == "1" then
                    val z = requiredZ / 26
                    val w = requiredZ - (z * 26) - chunks.head(14).t2.toLong
                    if w >= 1 && w <= 9 then List((w.toInt, z))
                    else Nil
                else Nil

            val parentValues = fromHigherNumber ++ fromSimilarNumber ++ fromSmallerNumber

            if chunks.tail.isEmpty then parentValues.maxByOption(_._1.toLong)(longOrder).map(_._1.toString)
            else
                parentValues
                    .flatMap: (w, z) =>
                        findAnswer(chunks.tail, z, longOrder).map(_ + w.toString)
                    .maxByOption(_.toLong)(longOrder)

        findAnswerCache.getOrElseUpdate(cacheKey, result)

    def parse(lines: List[String]): List[List[Operation]] =
        lines
            .flatMap {
                case s"inp $t1"     => None
                case s"$op $t1 $t2" => Some(Operation(t1, t2))
            }
            .grouped(17)
            .toList
