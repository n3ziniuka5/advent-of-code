package aoc

import aoc.Common.timed
import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

object Day24:
    sealed trait Operation:
        def t1: String
        def t2: String
        def out: String
    object Operation:
        case class AND(val t1: String, val t2: String, val out: String) extends Operation
        case class OR(val t1: String, val t2: String, val out: String)  extends Operation
        case class XOR(val t1: String, val t2: String, val out: String) extends Operation

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 24)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val (initial, operations) = parse(lines)
        val finalRegistry         = run(initial, topologicalSort(operations, initial))
        numberFromZs(finalRegistry)

    def part2(lines: List[String]): String =
        val (initial, operations) = parse(lines)
        val badOutputs =
            (findBadZOutputs(operations) ++ findWrongXYAnds(operations) ++ findWrongXYXors(operations)).distinct

        assert(badOutputs.size == 8)
        badOutputs.sorted.mkString(",")

    def run(registry: Map[String, Boolean], operations: List[Operation]): Map[String, Boolean] =
        operations match
            case head :: tail =>
                val newValue = head match
                    case Operation.AND(t1, t2, _) => registry(t1) && registry(t2)
                    case Operation.OR(t1, t2, _)  => registry(t1) || registry(t2)
                    case Operation.XOR(t1, t2, _) => registry(t1) != registry(t2)
                run(registry + (head.out -> newValue), tail)
            case Nil => registry

    def topologicalSort(operations: List[Operation], initialRegistry: Map[String, Boolean]): List[Operation] =
        @tailrec
        def sortLoop(remaining: List[Operation], result: List[Operation], resolvedDeps: Set[String]): List[Operation] =
            remaining match
                case head :: tail =>
                    if resolvedDeps.contains(head.out) then sortLoop(tail, result, resolvedDeps)
                    else if resolvedDeps.contains(head.t1) && resolvedDeps.contains(head.t2) then
                        sortLoop(tail, head +: result, resolvedDeps + head.out)
                    else
                        val deps = remaining.filter(op => op.out == head.t1 || op.out == head.t2)
                        sortLoop(deps ++ remaining, result, resolvedDeps)
                case Nil => result.reverse

        sortLoop(operations, Nil, initialRegistry.keySet)

    def numberFromZs(registry: Map[String, Boolean]): Long =
        val zString = registry.toList
            .filter(_._1.startsWith("z"))
            .sortBy(_._1)
            .map: (_, v) =>
                if v then "1" else "0"
            .mkString
            .reverse
        java.lang.Long.parseLong(zString, 2)

    def findBadZOutputs(operations: List[Operation]): List[String] =
        operations
            .filter(op => op.out.startsWith("z"))
            .filter(op => !op.isInstanceOf[Operation.XOR])
            .map(_.out)
            .filter(_ != "z45")

    def findWrongXYAnds(operations: List[Operation]): List[String] =
        operations
            .filter: op =>
                val number = op.t1.drop(1)
                (op.t1.startsWith("x") || op.t2.startsWith("x")) && op.isInstanceOf[Operation.AND] && number != "00"
            .filter: op =>
                operations
                    .find: op2 =>
                        (op2.t1 == op.out || op2.t2 == op.out) && op2.isInstanceOf[Operation.OR]
                    .isEmpty
            .map(_.out)

    def findWrongXYXors(operations: List[Operation]): List[String] =
        operations
            .filter: op =>
                val number = op.t1.drop(1)
                (op.t1.startsWith("x") || op.t2.startsWith("x")) && op.isInstanceOf[Operation.XOR] && number != "00"
            .flatMap: op =>
                val maybeLaterUsedInXor = operations.find: op2 =>
                    (op2.t1 == op.out || op2.t2 == op.out) && op2.isInstanceOf[Operation.XOR]

                maybeLaterUsedInXor match
                    case Some(laterXor) =>
                        val number = op.t1.drop(1)
                        if laterXor.out == s"z$number" then None else Some(laterXor.out)
                    case None => Some(op.out)

    def parse(lines: List[String]): (initial: Map[String, Boolean], operations: List[Operation]) =
        val initial = lines
            .takeWhile(_.nonEmpty)
            .map: line =>
                val Array(wire, valueStr) = line.split(": ")
                wire -> (valueStr == "1")
            .toMap

        val operations = lines
            .drop(initial.size + 1)
            .map:
                case s"$t1 XOR $t2 -> $out" => Operation.XOR(t1, t2, out)
                case s"$t1 OR $t2 -> $out"  => Operation.OR(t1, t2, out)
                case s"$t1 AND $t2 -> $out" => Operation.AND(t1, t2, out)
        (initial, operations)
