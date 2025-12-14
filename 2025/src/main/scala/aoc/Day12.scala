package aoc

import aoc.Common.timed
import collection.parallel.CollectionConverters.*

object Day12:
    type Shape = Map[Point, Char]
    case class Problem(width: Int, height: Int, toPlace: Vector[Int])
    case class Input(shapes: Vector[Shape], problems: List[Problem])

    case class MapWithStats(
        map: Map2d[Char],
        minX: Long,
        maxX: Long,
        minY: Long,
        maxY: Long
    )

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2025, 12)
        timed("Part 1", part1(lines))

    def parse(lines: List[String]): Input =
        def loopShapes(remainingLines: List[String], result: Vector[Shape]): Vector[Shape] =
            remainingLines match
                case _ :: _ =>
                    val shapeLines = remainingLines.drop(1).take(3)
                    val shape      = Map2d.fromLines(shapeLines).underlying
                    loopShapes(remainingLines.drop(5), result :+ shape)
                case Nil => result
        val shapes = loopShapes(lines.take(29), Vector.empty)

        val problems = lines
            .drop(30)
            .map:
                case s"${w}x${l}: $toPlace" => Problem(w.toInt, l.toInt, toPlace.split(" ").map(_.toInt).toVector)

        Input(shapes, problems)

    def rotate(shape: Shape): Shape =
        shape.map: (point, char) =>
            val newPoint = point match
                case Point(0, 0) => Point(2, 0)
                case Point(1, 0) => Point(2, 1)
                case Point(2, 0) => Point(2, 2)
                case Point(0, 1) => Point(1, 0)
                case Point(1, 1) => Point(1, 1)
                case Point(2, 1) => Point(1, 2)
                case Point(0, 2) => Point(0, 0)
                case Point(1, 2) => Point(0, 1)
                case Point(2, 2) => Point(0, 2)
                case _           => ???

            newPoint -> char

    def flip(shape: Shape): Shape =
        shape.map: (point, char) =>
            val newPoint = point.y match
                case 0 => Point(point.x, 2)
                case 2 => Point(point.x, 0)
                case _ => point

            newPoint -> char

    def placeShape(shape: Shape, mapWithStats: MapWithStats): Option[MapWithStats] =
        (for
            y <- 0L to mapWithStats.map.maxY
            x <- 0L to mapWithStats.map.maxX
        yield Point(x, y))
            .find: point =>
                (for
                    dy <- 0 to 2
                    dx <- 0 to 2
                yield (dx, dy)).forall: (dx, dy) =>
                    val mapPoint = Point(point.x + dx, point.y + dy)
                    mapWithStats.map.underlying
                        .contains(mapPoint) && (shape(Point(dx, dy)) == '.' || mapWithStats.map(mapPoint) == '.')
            .map: point =>
                val newMap = (for
                    dy <- 0 to 2
                    dx <- 0 to 2
                yield (dx, dy)).foldLeft(mapWithStats.map): (map, d) =>
                    val (dx, dy) = d
                    val mapPoint = Point(point.x + dx, point.y + dy)
                    if shape(Point(dx, dy)) == '#' then map.updated(mapPoint, '#') else map

                MapWithStats(
                  map = newMap,
                  minX = math.min(mapWithStats.minX, point.x),
                  maxX = math.max(mapWithStats.maxX, point.x + 2),
                  minY = math.min(mapWithStats.minY, point.y),
                  maxY = math.max(mapWithStats.maxY, point.y + 2),
                )

    def mostCompactMap(maps: Vector[(MapWithStats, Int)]): Option[(MapWithStats, Int)] =
        maps.minByOption: (map, _) =>
            (map.maxX - map.minX) * (map.maxY - map.minY)

    def canPlacePresents(problem: Problem, shapes: Vector[Shape]): Boolean =
        def place(mapWithStats: MapWithStats, toPlace: Vector[Int]): Boolean =
            val availableShapes = toPlace.zipWithIndex
                .filter(_._1 > 0)
                .flatMap: (_, shapeI) =>
                    val original = shapes(shapeI)
                    val flipped  = flip(original)

                    val originalRotations = (1 to 3).scanLeft(original)((shape, _) => rotate(shape))
                    val flippedRotations  = (1 to 3).scanLeft(flipped)((shape, _) => rotate(shape))

                    (originalRotations ++ flippedRotations).distinct.map((_, shapeI))

            if availableShapes.isEmpty then true
            else
                val newMaps =
                    availableShapes.flatMap((shape, shapeI) => placeShape(shape, mapWithStats).map(m => (m, shapeI)))
                val maybeBestMap = mostCompactMap(newMaps)
                maybeBestMap match
                    case Some((map, shapeI)) =>
                        val newToPlace = toPlace.updated(shapeI, toPlace(shapeI) - 1)
                        place(map, newToPlace)
                    case None => false

        val map = Map2d(
          (0 until problem.width)
              .flatMap: x =>
                  (0 until problem.height).map: y =>
                      Point(x, y) -> '.'
              .toMap
        )
        place(MapWithStats(map, Long.MaxValue, Long.MinValue, Long.MaxValue, Long.MinValue), problem.toPlace)

    def part1(lines: List[String]): Long =
        val input = parse(lines)

        input.problems.par
            .count: p =>
                val flipped = p.copy(width = p.height, height = p.width)
                canPlacePresents(p, input.shapes) || canPlacePresents(flipped, input.shapes)
