package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.language.experimental.namedTuples
import scala.collection.parallel.CollectionConverters.*

object Day6:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 6)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    enum Direction:
        case Up, Down, Left, Right

        def turn: Direction = this match
            case Up    => Right
            case Right => Down
            case Down  => Left
            case Left  => Up

    def part1(lines: List[String]): Long =
        val (map, start) = parse(lines)
        pointsTraveled(map, start, Direction.Up, Set.empty)

    def part2(lines: List[String]): Long =
        val (map, start) = parse(lines)
        numLoops(map, start)

    def parse(lines: List[String]): (map: Map2d[Char], start: Point) =
        val map   = Map2d.fromLines(lines)
        val start = map.underlying.find((_, v) => v == '^').get._1
        (Map2d(map.underlying.updated(start, '.')), start)

    def move(point: Point, direction: Direction): Point = direction match
        case Direction.Up    => point.up
        case Direction.Down  => point.down
        case Direction.Left  => point.left
        case Direction.Right => point.right

    def numLoops(
        map: Map2d[Char],
        startPoint: Point
    ): Int =
        @tailrec
        def collectSearches(
            current: Point,
            direction: Direction,
            placedWallsAt: Set[Point],
            result: List[(map: Map2d[Char], from: Point, direction: Direction)]
        ): List[(map: Map2d[Char], from: Point, direction: Direction)] =
            val next = move(current, direction)
            if map.get(next).contains('.') then
                if placedWallsAt.contains(next) then collectSearches(next, direction, placedWallsAt, result)
                else
                    val wallPlaced = Map2d.apply(map.underlying.updated(next, '#'))
                    collectSearches(next, direction, placedWallsAt + next, (wallPlaced, current, direction) +: result)
            else if map.get(next).contains('#') then collectSearches(current, direction.turn, placedWallsAt, result)
            else result

        collectSearches(startPoint, Direction.Up, Set.empty, List.empty).par.count: search =>
            isLooping(search.map, search.from, search.direction, Set.empty)

    @tailrec
    def isLooping(map: Map2d[Char], current: Point, direction: Direction, visited: Set[(Point, Direction)]): Boolean =
        // returns point before the wall
        def findNextWall(from: Point): Option[Point] =
            val next = move(from, direction)
            if map.get(next).contains('.') then findNextWall(next)
            else if map.get(next).contains('#') then Some(from)
            else None

        val maybeWallBefore = findNextWall(current)
        maybeWallBefore match
            case None => false
            case Some(wallBefore) =>
                if visited.contains((wallBefore, direction)) then true
                else isLooping(map, wallBefore, direction.turn, visited + ((wallBefore, direction)))

    @tailrec
    def pointsTraveled(
        map: Map2d[Char],
        current: Point,
        direction: Direction,
        visited: Set[Point]
    ): Int =
        val next       = move(current, direction)
        val newVisited = visited + current
        if map.get(next).contains('.') then pointsTraveled(map, next, direction, newVisited)
        else if map.get(next).contains('#') then pointsTraveled(map, current, direction.turn, newVisited)
        else newVisited.size
