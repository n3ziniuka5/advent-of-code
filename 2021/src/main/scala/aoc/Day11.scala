package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.MapView
import scala.io.Source

object Day11 {
  case class Position(x: Int, y: Int) {
    def neighbours: Set[Position] =
      (for {
        addX <- -1 to 1
        addY <- -1 to 1 if addX != 0 || addY != 0
      } yield Position(x + addX, y + addY)).toSet
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day11.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def constructMap(lines: List[String]): Map[Position, Int] = {
    lines.zipWithIndex.foldLeft(Map.empty[Position, Int]) { case (acc, (l, y)) =>
      acc ++ l.zipWithIndex.map { case (energy, x) =>
        Position(x, y) -> energy.toString.toInt
      }.toMap
    }
  }

  @tailrec
  def step(octopuses: Map[Position, Int], remaining: Int, flashes: Int = 0): (Int, Map[Position, Int]) = {
    remaining match {
      case 0 => (flashes, octopuses)
      case _ =>
        val withEnergyIncrease = octopuses.view.mapValues(_ + 1)

        @tailrec
        def flash(octopuses: MapView[Position, Int], flashes: Int = 0): (Int, MapView[Position, Int]) = {
          octopuses.find(_._2 > 9) match {
            case Some((pos, _)) =>
              val updatedNeighbours = pos.neighbours.flatMap { pos =>
                octopuses.get(pos) match {
                  case Some(ene) =>
                    val newEnergy = if (ene == 0) 0 else ene + 1
                    Some(pos -> newEnergy)
                  case None => None
                }
              }

              flash((octopuses ++ (updatedNeighbours + (pos -> 0))).toMap.view, flashes + 1)
            case None => (flashes, octopuses)
          }
        }

        val (newFlashes, newOctopuses) = flash(withEnergyIncrease)
        step(newOctopuses.toMap, remaining - 1, flashes + newFlashes)
    }
  }

  @tailrec
  def untilAllFlash(octopuses: Map[Position, Int], steps: Int = 0): Int = {
    if (octopuses.forall(_._2 == 0) && steps > 0) {
      steps
    } else {
      untilAllFlash(step(octopuses, 1)._2, steps + 1)
    }
  }

  def part1(lines: List[String]): Int = {
    step(constructMap(lines), 100)._1
  }

  def part2(lines: List[String]): Int = {
    untilAllFlash(constructMap(lines))
  }
}
