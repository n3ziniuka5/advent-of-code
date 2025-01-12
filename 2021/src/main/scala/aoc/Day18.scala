package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day18 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day18.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  sealed trait Element {
    def up: Option[Pair]

    override def toString: String = this match {
      case Pair(left, right, _) => s"[$left,$right]"
      case Literal(value, _)    => value.toString
    }
  }

  case class Literal(var value: Int, up: Option[Pair]) extends Element {
    def split: Unit = {
      val parent  = up.get
      val newPair = Pair(null, null, Some(parent))
      newPair.left = Literal(value / 2, Some(newPair))
      newPair.right = Literal((value.toDouble / 2).ceil.toInt, Some(newPair))

      if (parent.left.eq(this)) {
        parent.left = newPair
      } else {
        parent.right = newPair
      }
    }
  }

  case class Pair(var left: Element, var right: Element, var up: Option[Pair]) extends Element {
    def setTo0: Unit = {
      val parent     = up.get
      val newLiteral = Literal(0, Some(parent))

      if (parent.left.eq(this)) {
        parent.left = newLiteral
      } else {
        parent.right = newLiteral
      }
    }
  }

  def parseTree(line: String): Pair = {
    val root      = Pair(null, null, None)
    var ref       = root
    var addToLeft = true
    line.tail.dropRight(1).foreach {
      case '[' =>
        val newPair = Pair(null, null, Some(ref))
        if (addToLeft) ref.left = newPair else ref.right = newPair
        addToLeft = true
        ref = newPair
      case ']' =>
        ref = ref.up.get
      case ',' =>
        addToLeft = false
      case numberChar =>
        val newLiteral = Literal(numberChar.toString.toInt, Some(ref))
        if (addToLeft) ref.left = newLiteral else ref.right = newLiteral
    }

    ref
  }

  @tailrec
  def sideMostLiteral(element: Element, sideSelection: Pair => Element): Literal = {
    element match {
      case l: Literal => l
      case p: Pair    => sideMostLiteral(sideSelection(p), sideSelection)
    }
  }

  def closestLiteralToSide(
    pair: Pair,
    parentEqualitySide: Pair => Element,
    literalSide: Pair => Element
  ): Option[Literal] = {
    var maybeRef = Option(pair)

    while (maybeRef.nonEmpty && maybeRef.get.up.map(parentEqualitySide).exists(_.eq(maybeRef.get))) {
      maybeRef = maybeRef.get.up
    }

    maybeRef.flatMap(_.up) match {
      case Some(ref) =>
        Some(sideMostLiteral(parentEqualitySide(ref), literalSide))
      case None => None
    }
  }

  def closestLiteralToLeft(pair: Pair): Option[Literal] = {
    closestLiteralToSide(pair, _.left, _.right)
  }

  def closestLiteralToRight(pair: Pair): Option[Literal] = {
    closestLiteralToSide(pair, _.right, _.left)
  }

  def explode(pair: Pair): (Pair, Boolean) = {
    def loop(pair: Element, depth: Int): Boolean = {
      pair match {
        case p @ Pair(a: Literal, b: Literal, _) =>
          if (depth >= 4) {
            closestLiteralToLeft(p).foreach { l =>
              l.value = l.value + a.value
            }

            closestLiteralToRight(p).foreach { l =>
              l.value = l.value + b.value
            }

            p.setTo0

            true
          } else {
            false
          }
        case _: Literal => false
        case Pair(left, right, _) =>
          if (loop(left, depth + 1)) true else loop(right, depth + 1)
      }
    }

    (pair, loop(pair, 0))
  }

  def split(pair: Pair): (Pair, Boolean) = {
    def loop(pair: Element): Boolean = {
      pair match {
        case l: Literal if l.value >= 10 =>
          l.split
          true
        case Pair(left, right, _) =>
          if (loop(left)) true else loop(right)
        case _ => false
      }
    }

    (pair, loop(pair))
  }

  @tailrec
  def reduce(pair: Pair): Pair = {
    val (exploded, changed) = explode(pair)
    if (changed) {
      reduce(exploded)
    } else {
      val (afterSplit, changed) = split(exploded)
      if (changed) reduce(afterSplit) else afterSplit
    }
  }

  def sum(leftPair: Pair, rightPair: Pair): Pair = {
    val newPair = Pair(leftPair, rightPair, None)
    leftPair.up = Some(newPair)
    rightPair.up = Some(newPair)

    reduce(newPair)
  }

  def magnitute(element: Element): Int = {
    element match {
      case Literal(value, _)    => value
      case Pair(left, right, _) => 3 * magnitute(left) + 2 * magnitute(right)
    }
  }

  def part1(lines: List[String]): Int = {
    magnitute(lines.map(parseTree).reduce(sum))
  }

  def part2(lines: List[String]): Int = {
    val lineVector = lines.toVector
    val pairs = for {
      a <- lineVector.indices
      b <- lineVector.indices
    } yield (a, b)

    pairs
      .map((a, b) => (parseTree(lineVector(a)), parseTree(lineVector(b))))
      .map(sum)
      .map(magnitute)
      .max
  }
}
