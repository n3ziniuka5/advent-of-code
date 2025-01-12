package aoc

import aoc.Common.timed
import zio.prelude.MultiSet

import scala.annotation.tailrec
import scala.io.Source

object Day19 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day19.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  case class Point(x: Int, y: Int, z: Int)

  def readInput(lines: List[String]): Map[Int, List[Point]] = {
    @tailrec
    def loop(lines: List[String], scanner: Int = -1, acc: Map[Int, List[Point]] = Map.empty): Map[Int, List[Point]] = {
      lines match {
        case head :: tail =>
          if (head.contains("scanner")) {
            loop(tail, scanner + 1, acc)
          } else {
            val currentList    = acc.getOrElse(scanner, List.empty)
            val Array(x, y, z) = head.split(',').map(_.toInt)
            val updatedMap     = acc + (scanner -> (Point(x, y, z) +: currentList))
            loop(tail, scanner, updatedMap)
          }
        case Nil => acc
      }
    }

    loop(lines.filter(_.nonEmpty))
  }

  def possibleOrientationsForPoint(point: Point): List[Point] = {
    def axisToPoint(axis: String) = {
      axis match {
        case "x"  => point.x
        case "-x" => point.x * -1
        case "y"  => point.y
        case "-y" => point.y * -1
        case "z"  => point.z
        case "-z" => point.z * -1
      }
    }

    val axies = List("x", "y", "z")
    val b = (for {
      myXAxis   <- axies
      xModifier <- List(-1, 1)
      x = axisToPoint(myXAxis) * xModifier
      myYAxis   <- axies.filter(_ != myXAxis)
      yModifier <- List(-1, 1)
      y = axisToPoint(myYAxis) * yModifier
      myZAxis <- axies.filter(_ != myXAxis).filter(_ != myYAxis)
    } yield {
      List(
        Point(x, y, axisToPoint(myZAxis)),
        Point(x, y, axisToPoint(myZAxis) * -1)
        //Point(x, y, axisToPoint(myZAxis) * -1)
      )
    }).flatten

    //println(s"CCCCCCCCCCC ${b.size}")

    b

    /*val rotationAroundAxis = for {
      xMod <- List(-1, 1)
      x = point.x * xMod
      yMod <- List(-1, 1)
      y = point.y * yMod
    } yield Point(x, y, point.z)

    val rotationUpright = rotationAroundAxis.flatMap { p =>
      List(
        Point(p.x, p.y, p.z),
        Point(p.x, p.z, p.y * -1),
        Point(p.x, p.y * -1, p.z * -1),
        Point(p.x, p.z * -1, p.y)
      )
    }

    val rotationSideways = rotationUpright.flatMap { p =>
      List(
        Point(p.x, p.y, p.z),
        Point(p.z * -1, p.y, p.x),
        Point(p.x * -1, p.y, p.z * -1),
        Point(p.z, p.y, p.x * -1)
      )
    }

    rotationSideways*/
  }

  def possibleOrientations(points: List[Point]): List[List[Point]] = {
    val a = points.map(possibleOrientationsForPoint)

    for {
      numOrientations <- (0 until 48).toList
    } yield {
      a.map { b =>
        //println(s"${b.size} $numOrientations")
        b(numOrientations)
      }
    }
  }

  def compareBeaconLists(a: List[Point], b: List[Point]): Option[(Int, Int, Int)] = {
    val pairs = for {
      aa <- a
      bb <- b
    } yield (aa, bb)

    val d = pairs.foldLeft(MultiSet.apply[(Int, Int, Int)]()) { case (acc, (aa, bb)) =>
      acc <> MultiSet.apply((aa.x - bb.x, aa.y - bb.y, aa.z - bb.z))
    }

    val max = d.toMap.maxBy(_._2)
    /*if (max._2 != 1) {
      println(max)
    }*/

    if (max._2 >= 12) {
      Some(max._1)
    } else {
      None
    }
  }

  /*def reduceToSingleBeaconList(scanner0: List[Point], lists: Map[Int, List[Point]]): List[Point] = {
    println(lists.size)
    if (lists.isEmpty) {
      scanner0
    } else {
      val (mergedListId, listToMergeWithMods) = lists.view
        .mapValues { bPoints =>
          possibleOrientations(bPoints)
            .map { orientation =>
              (orientation, compareBeaconLists(scanner0, orientation))
            }
            .find(_._2.isDefined)
            .map { case (a, b) =>
              (a, b.get)
            }
        }
        .find(_._2.isDefined)
        .get

      val (listToMerge, mods) = listToMergeWithMods.get

      val newScanner = scanner0 ++ listToMerge.map(p =>
        p.copy(
          x = p.x + mods._1,
          y = p.y + mods._2,
          z = p.z + mods._3
        )
      )

      reduceToSingleBeaconList(newScanner.distinct, lists - mergedListId)

      //val newScanner = scanner0 ++ lis
    }
  }*/

  def reduceToSingleBeaconList(
    lists: Map[Int, List[Point]],
    beaconLocations: Map[Int, Point] = Map(0 -> Point(0, 0, 0))
  ): (List[Point], Map[Int, Point]) = {
    if (lists.size == 1) {
      (lists.head._2, beaconLocations)
    } else {
      /*if (lists.size == 9) {
        lists.foreach { case (i, _) =>
          println(s"LIST $i has ${lists(i).size} beacons")
        }
      }*/

      val (indexA, indexB, mods) = LazyList
        .from(
          lists.keySet.toList.combinations(2).map { comb =>
            //println(s"COMPARING ${comb(0)} ${comb(1)}")
            (comb(0), comb(1))
          }
        )
        .map { case (indexA, indexB) =>
          val possibleModification = possibleOrientations(lists(indexB))
            .map { orientation =>
              (orientation, compareBeaconLists(lists(indexA), orientation))
            }
            .find(_._2.isDefined)
            .map { case (a, b) =>
              (a, b.get)
            }
          (indexA, indexB, possibleModification)
        }
        .find(_._3.isDefined)
        .get

      val newIndexA = lists(indexA) ++ mods.get._1.map(p =>
        p.copy(
          x = p.x + mods.get._2._1,
          y = p.y + mods.get._2._2,
          z = p.z + mods.get._2._3
        )
      )

      reduceToSingleBeaconList(
        lists + (indexA           -> newIndexA.distinct) - indexB,
        beaconLocations + (indexB -> Point(mods.get._2._1, mods.get._2._2, mods.get._2._3))
      )

      /*

      val (mergedListId, listToMergeWithMods) = lists.view
        .mapValues { bPoints =>
          possibleOrientations(bPoints)
            .map { orientation =>
              (orientation, compareBeaconLists(scanner0, orientation))
            }
            .find(_._2.isDefined)
            .map { case (a, b) =>
              (a, b.get)
            }
        }
        .find(_._2.isDefined)
        .get

      val (listToMerge, mods) = listToMergeWithMods.get

      val newScanner = scanner0 ++ listToMerge.map(p =>
        p.copy(
          x = p.x + mods._1,
          y = p.y + mods._2,
          z = p.z + mods._3
        )
      )

      reduceToSingleBeaconList(newScanner.distinct, lists - mergedListId)*/

      //val newScanner = scanner0 ++ lis
    }
  }

  def part1(lines: List[String]): Int = {
    val input = readInput(lines)

    val answer = reduceToSingleBeaconList(input)

    answer._1.size

    /*val scanner0 = input(0)
    val scanner1 = input(1)

    val scanner1InDifferentOrientations = possibleOrientations(scanner1)

    def loop(a: List[Point], b: List[List[Point]]): List[Point] = {
      b.headOption.match {
        case Some(orientation) =>
          compareBeaconLists(a, orientation) match {
            case Some(xMod, yMod, zMod) =>
              println("FOUND SOMETHING")

              (a ++ orientation.map(p => p.copy(
                x = p.x + xMod,
                y = p.y + yMod,
                z = p.z + zMod
              ))).distinct
            case None => loop(a, b.tail)
          }
        case None => a
      }
    }

    val a1 = loop(scanner0, scanner1InDifferentOrientations)

    val a2 = loop(a1, possibleOrientations(input(3)))
    val a3 = loop(a2, possibleOrientations(input(4)))
    val a4 = loop(a3, possibleOrientations(input(2)))

    a4.size*/
  }

  def part2(lines: List[String]): Int = {
    val input = readInput(lines)

    val (_, answer) = reduceToSingleBeaconList(input)

    answer.keySet.toList
      .combinations(2)
      .map { comb =>
        val a = answer(comb(0))
        val b = answer(comb(1))

        Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z)
      }
      .max
  }
}
