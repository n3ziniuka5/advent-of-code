package aoc

import aoc.Common.timed
import language.experimental.namedTuples
import scala.collection.parallel.CollectionConverters.*

object Day22:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 22)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        lines.map(s => tickSecret(s.toLong, 2000)).sum

    def part2(lines: List[String]): Long =
        mostPossibleBananas(lines.map(_.toLong))

    def mostPossibleBananas(initialSecrets: List[Long]): Long =
        val pricesPerMonkey = initialSecrets.map: secret =>
            (1 to 2000).toVector
                .scanLeft(secret): (acc, _) =>
                    tickSecret(acc, 1)
                .map(_.toString.last.toString.toLong)

        val deltasPerMoneky = pricesPerMonkey.map: prices =>
            prices.indices
                .drop(1)
                .map: i =>
                    val delta = prices(i) - prices(i - 1)
                    (prices(i), delta)

        val deltaSequenceToBuyPerMonkey = deltasPerMoneky.map: deltas =>
            deltas.indices
                .drop(3)
                .foldLeft(Map.empty[String, Long]): (acc, i) =>
                    val purchaseSequence =
                        s"${deltas(i - 3)._2}|${deltas(i - 2)._2}|${deltas(i - 1)._2}|${deltas(i)._2}"
                    if acc.contains(purchaseSequence) then acc
                    else acc + (purchaseSequence -> deltas(i)._1)

        val uniqueSequences = deltaSequenceToBuyPerMonkey.flatMap(_.keySet).distinct

        uniqueSequences.par
            .map: sequence =>
                deltaSequenceToBuyPerMonkey.map(_.getOrElse(sequence, 0L)).sum
            .max

    def tickSecret(secret: Long, times: Long): Long =
        val first  = (secret ^ (secret * 64))   % 16777216
        val second = (first ^ (first / 32))     % 16777216
        val result = (second ^ (second * 2048)) % 16777216

        if times == 1 then result
        else tickSecret(result, times - 1)
